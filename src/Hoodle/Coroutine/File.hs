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

import           Control.Applicative
import           Control.Category
import           Control.Monad.State
import           Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy as L
-- import           Data.Label
import           Control.Lens
import           Graphics.UI.Gtk hiding (get,set)
import           System.Directory
import           System.FilePath
-- from hoodle-platform
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple
import           Text.Hoodle.Builder 
-- from this package 
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Commit
import           Hoodle.ModelAction.File
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Window
import qualified Hoodle.Script.Coroutine as S
import           Hoodle.Script.Hook
import           Hoodle.Type.Coroutine
import           Hoodle.Type.HoodleState
--
import Prelude hiding ((.),id)

-- | 
askIfSave :: MainCoroutine () -> MainCoroutine () 
askIfSave action = do 
    xstate <- get 
    if not (view isSaved xstate)
      then do 
        dialog <- liftIO $ messageDialogNew Nothing [DialogModal] 
          MessageQuestion ButtonsOkCancel 
          "Current canvas is not saved yet. Will you proceed without save?" 
        res <- liftIO $ dialogRun dialog
        case res of
          ResponseOk -> do liftIO $ widgetDestroy dialog
                           action
          _ -> do liftIO $ widgetDestroy dialog
        return () 
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
    {- let action = Left . ActionOrder $ 
                   \evhandler -> do 
                      forkIO $ do threadDelay 10000000
                                  putStrLn "BAAAAAM"
                                  evhandler (Menu MenuQuit)
                      return () 
    modify (tempQueue %~ enqueue action) -}
    xstate <- get 
    case view currFileName xstate of
      Nothing -> fileSaveAs 
      Just filename -> do     
        let hdlmodst = view hoodleModeState xstate
        let hdl = case hdlmodst of 
              ViewAppendState hdlmap -> Hoodle <$> view g_title <*> gToList . fmap (toPageFromBuf gToBackground) . view g_pages $ hdlmap 
              SelectState thdl -> Hoodle <$> gselectTitle <*> gToList . fmap (toPageFromBuf gToBackground) . gselectAll $ thdl 
        liftIO . L.writeFile filename . builder $ hdl
        put . set isSaved True $ xstate 
        let ui = view gtkUIManager xstate
        liftIO $ toggleSave ui False
        S.afterSaveHook hdl

-- | main coroutine for open a file 
fileOpen :: MainCoroutine ()
fileOpen = do 
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
            xstate <- get 
            xstate' <- liftIO $ getFileContent (Just filename) xstate
            ncvsinfo <- liftIO $ setPage xstate' 0 (getCurrentCanvasId xstate')
            xstateNew <- return $ modifyCurrentCanvasInfo (const ncvsinfo) xstate'
            put . set isSaved True 
                  $ xstateNew 
            liftIO $ setTitleFromFileName xstateNew  
            clearUndoHistory 
            invalidateAll 
        liftIO $ widgetDestroy dialog
      ResponseCancel -> liftIO $ widgetDestroy dialog
      _ -> error "??? in fileOpen " 
    return ()

-- | main coroutine for save as 
fileSaveAs :: MainCoroutine () 
fileSaveAs = do 
    xstate <- get 
    let hdlmodst = view hoodleModeState xstate
    let hdl = case hdlmodst of 
          ViewAppendState hdlmap -> gcast hdlmap
          SelectState thdl -> gcast thdl
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
              let ntitle = B.pack . snd . splitFileName $ filename 
              let (hdlmodst',hdl') = case view hoodleModeState xstate of
                    ViewAppendState hdlmap -> 
                      if view g_title hdlmap == "untitled"
                      then ( ViewAppendState . set g_title ntitle
                             $ hdlmap
                           , (set s_title ntitle hdl))
                      else (ViewAppendState hdlmap,hdl)
                    SelectState thdl -> 
                      if gselectTitle thdl == "untitled"
                      then ( SelectState $ 
                               thdl { gselectTitle = ntitle }
                           , set s_title ntitle hdl)  
                      else (SelectState thdl,hdl)
              let xstateNew = set currFileName (Just filename) 
                              . set hoodleModeState hdlmodst' $ xstate 
              liftIO . L.writeFile filename . builder $ hdl'
              put . set isSaved True $ xstateNew    
              let ui = view gtkUIManager xstateNew
              liftIO $ toggleSave ui False
              liftIO $ setTitleFromFileName xstateNew 
              S.afterSaveHook hdl'
          liftIO $ widgetDestroy dialog
        ResponseCancel -> liftIO $ widgetDestroy dialog
        _ -> error "??? in fileSaveAs"
      return ()

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
      _ -> error "??? in fileOpen " 
    return ()




