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
import           Control.Monad.Trans
import           Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy as L
import           Data.Label
import           Graphics.UI.Gtk hiding (get,set)
import           System.Directory
import           System.FilePath
import           Text.Xournal.Builder 
-- from hoodle-platform
import           Data.Xournal.Generic
import           Data.Xournal.Simple
-- from this package 
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Commit
import           Hoodle.ModelAction.File
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Window
import qualified Hoodle.Script.Coroutine as S
import           Hoodle.Script.Hook
import           Hoodle.Type.Coroutine
import           Hoodle.Type.XournalState
--
import Prelude hiding ((.),id)

-- | 
askIfSave :: MainCoroutine () -> MainCoroutine () 
askIfSave action = do 
    xstate <- getSt 
    if not (get isSaved xstate)
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
    xstate <- getSt
    xstate' <- liftIO $ getFileContent Nothing xstate 
    ncvsinfo <- liftIO $ setPage xstate' 0 (getCurrentCanvasId xstate')
    xstate'' <- return $ modifyCurrentCanvasInfo (const ncvsinfo) xstate'
    liftIO $ setTitleFromFileName xstate''
    commit xstate'' 
    invalidateAll 

-- | 
fileSave :: MainCoroutine ()
fileSave = do 
    xstate <- getSt 
    case get currFileName xstate of
      Nothing -> fileSaveAs 
      Just filename -> do     
        let xojstate = get xournalstate xstate
        let xoj = case xojstate of 
              ViewAppendState xojmap -> Xournal <$> get g_title <*> gToList . fmap (toPageFromBuf gToBackground) . get g_pages $ xojmap 
              SelectState txoj -> Xournal <$> gselectTitle <*> gToList . fmap (toPageFromBuf gToBackground) . gselectAll $ txoj 
        liftIO . L.writeFile filename . builder $ xoj
        putSt . set isSaved True $ xstate 
        let ui = get gtkUIManager xstate
        liftIO $ toggleSave ui False
        S.afterSaveHook xoj

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
            xstate <- getSt 
            xstate' <- liftIO $ getFileContent (Just filename) xstate
            ncvsinfo <- liftIO $ setPage xstate' 0 (getCurrentCanvasId xstate')
            xstateNew <- return $ modifyCurrentCanvasInfo (const ncvsinfo) xstate'
            putSt . set isSaved True 
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
    xstate <- getSt 
    let xojstate = get xournalstate xstate
    let xoj = case xojstate of 
          ViewAppendState xojmap -> gcast xojmap
          SelectState txoj -> gcast txoj
    maybe (defSaveAsAction xstate xoj) (\f -> liftIO (f xoj))
          (hookSaveAsAction xstate) 
  where 
    hookSaveAsAction xstate = do 
      hset <- get hookSet xstate
      saveAsHook hset
    defSaveAsAction xstate xoj = do 
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
              let (xojstate',xoj') = case get xournalstate xstate of
                    ViewAppendState xojmap -> 
                      if get g_title xojmap == "untitled"
                      then ( ViewAppendState . set g_title ntitle
                             $ xojmap
                           , (set s_title ntitle xoj))
                      else (ViewAppendState xojmap,xoj)
                    SelectState txoj -> 
                      if gselectTitle txoj == "untitled"
                      then ( SelectState $ 
                               txoj { gselectTitle = ntitle }
                           , set s_title ntitle xoj)  
                      else (SelectState txoj,xoj)
              let xstateNew = set currFileName (Just filename) 
                              . set xournalstate xojstate' $ xstate 
              liftIO . L.writeFile filename . builder $ xoj'
              putSt . set isSaved True $ xstateNew    
              let ui = get gtkUIManager xstateNew
              liftIO $ toggleSave ui False
              liftIO $ setTitleFromFileName xstateNew 
              S.afterSaveHook xoj'
          liftIO $ widgetDestroy dialog
        ResponseCancel -> liftIO $ widgetDestroy dialog
        _ -> error "??? in fileSaveAs"
      return ()

-- | 
fileAnnotatePDF :: MainCoroutine ()
fileAnnotatePDF = do 
    xstate <- getSt
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
            mxoj <- liftIO $ makeNewXojWithPDF filename 
            flip (maybe (return ())) mxoj $ \xoj -> do 
              xstateNew <- return . set currFileName Nothing 
                           =<< (liftIO $ constructNewHoodleStateFromXournal xoj xstate)
              commit xstateNew 
              liftIO $ setTitleFromFileName xstateNew             
              invalidateAll  
        liftIO $ widgetDestroy dialog
      ResponseCancel -> liftIO $ widgetDestroy dialog
      _ -> error "??? in fileOpen " 
    return ()




