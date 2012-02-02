-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Coroutine.File 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.Coroutine.File where

import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.XournalState
import Application.HXournal.Accessor
import Application.HXournal.Coroutine.Draw
import Application.HXournal.Coroutine.Commit
import Application.HXournal.ModelAction.Window
import Application.HXournal.ModelAction.Page
import Application.HXournal.ModelAction.File
import Text.Xournal.Builder 
import Control.Monad.Trans
import Control.Applicative
import Data.Xournal.Generic

import Graphics.UI.Gtk hiding (get,set)
import Control.Category
import Data.Label
import Prelude hiding ((.),id)
import qualified Data.ByteString.Lazy as L

import Data.Xournal.Simple


import System.Directory

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

fileNew :: MainCoroutine () 
fileNew = do  
    xstate <- getSt
    xstate' <- liftIO $ getFileContent Nothing xstate 
    ncvsinfo <- liftIO $ setPage xstate' 0 (get currentCanvasId xstate')
    xstate'' <- return $ modifyCurrentCanvasInfo (const ncvsinfo) xstate'
    liftIO $ setTitleFromFileName xstate''
    commit xstate'' 
    invalidateAll 


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

-- | main coroutine for open a file 

fileOpen :: MainCoroutine ()
fileOpen = do 
    liftIO $ putStrLn "file open clicked"
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
            liftIO $ putStrLn $ show filename 
            xstate <- getSt 
            xstate' <- liftIO $ getFileContent (Just filename) xstate
            ncvsinfo <- liftIO $ setPage xstate' 0 (get currentCanvasId xstate')
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
    liftIO $ putStrLn "file save as clicked"
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
            liftIO $ putStrLn $ show filename 
            xstate <- getSt 
            let xstateNew = set currFileName (Just filename) xstate 
            let xojstate = get xournalstate xstateNew
            let xoj = case xojstate of 
                  ViewAppendState xojmap -> Xournal 
                                            <$> get g_title 
                                            <*> gToList . fmap (toPageFromBuf gToBackground) . get g_pages 
                                            $ xojmap 
                  SelectState txoj -> Xournal <$> gselectTitle <*> gToList . fmap (toPageFromBuf gToBackground) . gselectAll $ txoj 
            liftIO . L.writeFile filename . builder $ xoj
            putSt . set isSaved True $ xstateNew    
            let ui = get gtkUIManager xstateNew
            liftIO $ toggleSave ui False
            liftIO $ setTitleFromFileName xstateNew 
        liftIO $ widgetDestroy dialog
      ResponseCancel -> liftIO $ widgetDestroy dialog
      _ -> error "??? in fileSaveAs"
    return ()



fileAnnotatePDF :: MainCoroutine ()
fileAnnotatePDF = do 
    xstate <- getSt
    liftIO $ putStrLn "file annotate PDf clicked"
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
            liftIO $ putStrLn $ show filename 
            mxoj <- liftIO $ makeNewXojWithPDF filename 
            flip (maybe (return ())) mxoj $ \xoj -> do 
              xstateNew <- return . set currFileName Nothing 
                           =<< (liftIO $ constructNewHXournalStateFromXournal xoj xstate)
              commit xstateNew 
              liftIO $ setTitleFromFileName xstateNew             
              invalidateAll  
        liftIO $ widgetDestroy dialog
      ResponseCancel -> liftIO $ widgetDestroy dialog
      _ -> error "??? in fileOpen " 
    return ()
