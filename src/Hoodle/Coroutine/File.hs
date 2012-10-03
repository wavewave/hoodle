{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.File 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.File where

-- from other packages
import           Control.Category
import           Control.Lens
import           Control.Monad.State
import           Data.ByteString.Char8 as B (pack,unpack)
import qualified Data.ByteString.Lazy as L
import qualified Data.IntMap as IM
import           Graphics.UI.Gtk hiding (get,set)
import           System.Directory
import           System.FilePath
-- from hoodle-platform
import           Control.Monad.Trans.Crtn.Event
import           Control.Monad.Trans.Crtn.Queue 
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple
import           Data.Hoodle.Select
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Item
import           Graphics.Hoodle.Render.Type
import           Text.Hoodle.Builder 
-- from this package 
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Commit
import           Hoodle.ModelAction.File
import           Hoodle.ModelAction.Layer 
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Window
import qualified Hoodle.Script.Coroutine as S
import           Hoodle.Script.Hook
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
--
import Prelude hiding ((.),id)

-- | 
okCancelMessageBox :: String -> MainCoroutine Bool 
okCancelMessageBox msg = modify (tempQueue %~ enqueue action) >> go 
  where 
    action = Left . ActionOrder $ 
               \evhandler -> do 
                 dialog <- messageDialogNew Nothing [DialogModal]
                   MessageQuestion ButtonsOkCancel msg 
                 res <- dialogRun dialog 
                 let b = case res of 
                           ResponseOk -> True
                           _ -> False
                 widgetDestroy dialog 
                 return (OkCancel b)
    go = do r <- nextevent                   
            case r of 
              OkCancel b -> return b  
              _ -> go 

-- | 
fileChooser :: MainCoroutine (Maybe FilePath) 
fileChooser = modify (tempQueue %~ enqueue action) >> go 
  where 
    go = do r <- nextevent                   
            case r of 
              FileChosen b -> return b  
              _ -> go 
    action = Left . ActionOrder $ \evhandler -> do 
      dialog <- fileChooserDialogNew Nothing Nothing 
                  FileChooserActionOpen 
                  [ ("OK", ResponseOk) 
                  , ("Cancel", ResponseCancel) ]
      cwd <- getCurrentDirectory                  
      fileChooserSetCurrentFolder dialog cwd 
      res <- dialogRun dialog
      mr <- case res of 
              ResponseDeleteEvent -> return Nothing
              ResponseOk ->  fileChooserGetFilename dialog 
              ResponseCancel -> return Nothing 
              _ -> putStrLn "??? in fileOpen" >> return Nothing 
      widgetDestroy dialog
      return (FileChosen mr)

-- | 
askIfSave :: MainCoroutine () -> MainCoroutine () 
askIfSave action = do 
    xstate <- get 
    if not (view isSaved xstate)
      then do  
        b <- okCancelMessageBox "Current canvas is not saved yet. Will you proceed without save?" 
        case b of 
          True -> action 
          False -> return () 
      else action 

-- | 
fileNew :: MainCoroutine () 
fileNew = do  
    xstate <- get
    xstate' <- liftIO $ getFileContent Nothing xstate 
    ncvsinfo <- liftIO $ setPage xstate' 0 (getCurrentCanvasId xstate')
    xstate'' <- return $ modifyCurrentCanvasInfo (const ncvsinfo) xstate'
    liftIO $ setTitleFromFileName xstate''
    commit xstate'' 
    invalidateAll 

-- | 
fileSave :: MainCoroutine ()
fileSave = do 
    xstate <- get 
    case view currFileName xstate of
      Nothing -> fileSaveAs 
      Just filename -> do     
        -- this is rather temporary not to make mistake 
        if takeExtension filename == ".hdl" 
          then do 
             let hdl = (rHoodle2Hoodle . getHoodle) xstate 
             liftIO . L.writeFile filename . builder $ hdl
             put . set isSaved True $ xstate 
             let ui = view gtkUIManager xstate
             liftIO $ toggleSave ui False
             S.afterSaveHook hdl
           else fileExtensionInvalid >> fileSaveAs 

-- | 
fileLoad :: FilePath -> MainCoroutine () 
fileLoad filename = do
    xstate <- get 
    xstate' <- liftIO $ getFileContent (Just filename) xstate
    ncvsinfo <- liftIO $ setPage xstate' 0 (getCurrentCanvasId xstate')
    xstateNew <- return $ modifyCurrentCanvasInfo (const ncvsinfo) xstate'
    put . set isSaved True 
      $ xstateNew 
    liftIO $ setTitleFromFileName xstateNew  
    clearUndoHistory 
    invalidateAll 


-- | main coroutine for open a file 
fileOpen :: MainCoroutine ()
fileOpen = do 
  mfilename <- fileChooser 
  case mfilename of 
    Nothing -> return ()
    Just filename -> fileLoad filename 
{-    cwd <- liftIO getCurrentDirectory
    dialog <- liftIO $ fileChooserDialogNew Nothing Nothing 
                                            FileChooserActionOpen 
                                            [ ("OK", ResponseOk) 
                                            , ("Cancel", ResponseCancel) ]
    liftIO $ fileChooserSetCurrentFolder dialog cwd 
    res <- liftIO $ dialogRun dialog
    case res of 
      ResponseDeleteEvent -> liftIO $ widgetDestroy dialog
      ResponseOk ->  do
        mfilename <- liftIO $ fileChooserGetFilename dialog 
        case mfilename of 
          Nothing -> return () 
          Just filename -> fileLoad filename 
        liftIO $ widgetDestroy dialog
      ResponseCancel -> liftIO $ widgetDestroy dialog
      _ -> error "??? in fileOpen " 
    return () -}

-- | main coroutine for save as 
fileSaveAs :: MainCoroutine () 
fileSaveAs = do 
    xstate <- get 
    let hdl = (rHoodle2Hoodle . getHoodle) xstate
    maybe (defSaveAsAction xstate hdl) (\f -> liftIO (f hdl))
          (hookSaveAsAction xstate) 
  where 
    hookSaveAsAction xstate = do 
      hset <- view hookSet xstate
      saveAsHook hset
    defSaveAsAction xstate hdl = do 
      cwd <- liftIO getCurrentDirectory
      dialog <- liftIO $ fileChooserDialogNew Nothing Nothing 
                                              FileChooserActionSave 
                                              [ ("OK", ResponseOk) 
                                              , ("Cancel", ResponseCancel) ]
      liftIO $ fileChooserSetCurrentFolder dialog cwd 
      res <- liftIO $ dialogRun dialog
      case res of 
        ResponseDeleteEvent -> liftIO $ widgetDestroy dialog
        ResponseOk -> do
          mfilename <- liftIO $ fileChooserGetFilename dialog 
          case mfilename of 
            Nothing -> return () 
            Just filename -> do 
              if takeExtension filename == ".hdl" 
                then do 
                  let ntitle = B.pack . snd . splitFileName $ filename 
                  let (hdlmodst',hdl') = case view hoodleModeState xstate of
                        ViewAppendState hdlmap -> 
                          if view gtitle hdlmap == "untitled"
                            then ( ViewAppendState . set gtitle ntitle
                                   $ hdlmap
                                 , (set title ntitle hdl))
                            else (ViewAppendState hdlmap,hdl)
                        SelectState thdl -> 
                          if view gselTitle thdl == "untitled"
                            then ( SelectState $ set gselTitle ntitle thdl 
                                 , set title ntitle hdl)  
                            else (SelectState thdl,hdl)
                  let xstateNew = set currFileName (Just filename) 
                                . set hoodleModeState hdlmodst' $ xstate 
                  liftIO . L.writeFile filename . builder $ hdl'
                  put . set isSaved True $ xstateNew    
                  let ui = view gtkUIManager xstateNew
                  liftIO $ toggleSave ui False
                  liftIO $ setTitleFromFileName xstateNew 
                  S.afterSaveHook hdl'
                else fileExtensionInvalid
          liftIO $ widgetDestroy dialog
        ResponseCancel -> liftIO $ widgetDestroy dialog
        _ -> error "??? in fileSaveAs"
      return ()

-- | main coroutine for open a file 
fileReload :: MainCoroutine ()
fileReload = do 
    xstate <- get
    case view currFileName xstate of 
      Nothing -> return () 
      Just filename -> do
        if not (view isSaved xstate) 
          then do
            b <- okCancelMessageBox "Discard changes and reload the file?" 
            case b of 
              True -> fileLoad filename 
              False -> return ()
          else fileLoad filename

-- | 
fileExtensionInvalid :: MainCoroutine ()
fileExtensionInvalid = do 
    dialog <- liftIO $ messageDialogNew Nothing [DialogModal]
                         MessageQuestion ButtonsOk
                         "only .hdl extension is supported for save"
    _res <- liftIO $ dialogRun dialog
    liftIO $ widgetDestroy dialog



-- | 
fileAnnotatePDF :: MainCoroutine ()
fileAnnotatePDF = do 
    xstate <- get
    cwd <- liftIO getCurrentDirectory
    dialog <- liftIO $ fileChooserDialogNew Nothing Nothing 
                                            FileChooserActionOpen 
                                            [ ("OK", ResponseOk) 
                                            , ("Cancel", ResponseCancel) ]
    liftIO $ fileChooserSetCurrentFolder dialog cwd
    res <- liftIO $ dialogRun dialog
    case res of 
      ResponseDeleteEvent -> liftIO $ widgetDestroy dialog
      ResponseOk ->  do
        mfilename <- liftIO $ fileChooserGetFilename dialog 
        case mfilename of 
          Nothing -> return () 
          Just filename -> do 
            mhdl <- liftIO $ makeNewHoodleWithPDF filename 
            flip (maybe (return ())) mhdl $ \hdl -> do 
              xstateNew <- return . set currFileName Nothing 
                           =<< (liftIO $ constructNewHoodleStateFromHoodle hdl xstate)
              commit xstateNew 
              liftIO $ setTitleFromFileName xstateNew             
              invalidateAll  
        liftIO $ widgetDestroy dialog
      ResponseCancel -> liftIO $ widgetDestroy dialog
      _ -> error "??? in fileAnnoPDF " 
    return ()


-- | 
fileLoadImage :: MainCoroutine ()
fileLoadImage = do 
    xstate <- get
    cwd <- liftIO getCurrentDirectory
    dialog <- liftIO $ fileChooserDialogNew Nothing Nothing 
                                            FileChooserActionOpen 
                                            [ ("OK", ResponseOk) 
                                            , ("Cancel", ResponseCancel) ]
    liftIO $ fileChooserSetCurrentFolder dialog cwd
    res <- liftIO $ dialogRun dialog
    case res of 
      ResponseDeleteEvent -> liftIO $ widgetDestroy dialog
      ResponseOk ->  do
        mfilename <- liftIO $ fileChooserGetFilename dialog 
        case mfilename of 
          Nothing -> return () 
          Just filename -> do 
            liftIO $ putStrLn filename 
            let pgnum = unboxGet currentPageNum . view currentCanvasInfo $ xstate
                hdl = getHoodle xstate 
                (mcurrlayer,currpage) = getCurrentLayerOrSet (getPageFromGHoodleMap pgnum hdl)
                currlayer = maybe (error "something wrong in addPDraw") id mcurrlayer 
            newitem <- (liftIO . cnstrctRItem . ItemImage) 
                         (Image (B.pack filename) (100,100) (Dim 300 300))
            newlayerbbox <- liftIO 
                            . updateLayerBuf (Just (getBBox newitem))
                            . over gitems (++[newitem]) 
                            $ currlayer
            let newpagebbox = adjustCurrentLayer newlayerbbox currpage 
                newhdlbbox = set gpages (IM.adjust (const newpagebbox) pgnum (view gpages hdl) ) hdl 
            let xstateNew = set hoodleModeState (ViewAppendState newhdlbbox) xstate
            put xstateNew 
            invalidateAll 
        liftIO $ widgetDestroy dialog
      ResponseCancel -> liftIO $ widgetDestroy dialog
      _ -> error "??? in fileLoadImage " 
    return ()




