module Application.HXournal.Iteratee.File where

import Application.HXournal.Type.Event
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.XournalState
import Application.HXournal.Accessor
import Application.HXournal.ModelAction.File 
import Application.HXournal.Iteratee.Draw
import Application.HXournal.ModelAction.Page
import Application.HXournal.Builder 

import Control.Monad.Trans
import Control.Applicative

import Text.Xournal.Type
import Graphics.Xournal.Type.Map
import Graphics.Xournal.Type.Select
import Graphics.UI.Gtk hiding (get,set)

import Control.Category
import Data.Label
import Prelude hiding ((.),id)

import qualified Data.ByteString.Lazy as L
import qualified Data.IntMap as M


fileNew :: Iteratee MyEvent XournalStateIO ()
fileNew = do 
    liftIO $ putStrLn "fileNew called"
    xstate <- getSt 
    let newxoj = mkXournalBBoxMapFromXournal defaultXournal 
        newxojstate = ViewAppendState newxoj 
    let xstate' = set currFileName Nothing 
                  . set xournalstate newxojstate
                  $ xstate 
        cmap = get canvasInfoMap xstate'
        cmap' = M.map (setPage newxojstate 0) cmap
        xstate'' = set canvasInfoMap cmap' xstate'
    putSt xstate'' 
    invalidateAll 

fileSave :: Iteratee MyEvent XournalStateIO () 
fileSave = do 
    xstate <- getSt 
    case get currFileName xstate of
      Nothing -> fileSaveAs -- "mytest.xoj"
      Just filename -> do     
        let xojstate = get xournalstate xstate
        let xoj = case xojstate of 
                    ViewAppendState xojmap -> xournalFromXournalBBoxMap xojmap 
                    SelectState txoj -> xournalFromXournalBBoxMap 
                                        $ XournalBBoxMap <$> tx_pages $ txoj 
        liftIO . L.writeFile filename . builder $ xoj

fileOpen :: Iteratee MyEvent XournalStateIO ()
fileOpen = do 
    liftIO $ putStrLn "file open clicked"
    dialog <- liftIO $ fileChooserDialogNew Nothing Nothing 
                                            FileChooserActionOpen 
                                            [ ("OK", ResponseOk) 
                                            , ("Cancel", ResponseCancel) ]
    response <- liftIO $ dialogRun dialog
    liftIO $ putStrLn $ show response
    case response of 
      ResponseDeleteEvent -> liftIO $ widgetDestroy dialog
      ResponseOk ->  do
        mfilename <- liftIO $ fileChooserGetFilename dialog 
        case mfilename of 
          Nothing -> return () 
          Just filename -> do 
            liftIO $ putStrLn $ show filename 
            xstate <- getSt 
            xstateNew <- liftIO $ getFileContent filename xstate
            putSt xstateNew 
            invalidateAll 
        liftIO $ widgetDestroy dialog
      ResponseCancel -> liftIO $ widgetDestroy dialog
      _ -> error "??? in fileOpen " 
    return ()

fileSaveAs :: Iteratee MyEvent XournalStateIO ()
fileSaveAs = do 
    liftIO $ putStrLn "file save as clicked"
    dialog <- liftIO $ fileChooserDialogNew Nothing Nothing 
                                            FileChooserActionSave 
                                            [ ("OK", ResponseOk) 
                                            , ("Cancel", ResponseCancel) ]
    response <- liftIO $ dialogRun dialog
    liftIO $ putStrLn $ show response
    case response of 
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
                        ViewAppendState xojmap -> xournalFromXournalBBoxMap xojmap 
                        SelectState txoj -> xournalFromXournalBBoxMap 
                                            $ XournalBBoxMap <$> tx_pages $ txoj 
            liftIO . L.writeFile filename . builder $ xoj
            putSt xstateNew                                     
        liftIO $ widgetDestroy dialog
      ResponseCancel -> liftIO $ widgetDestroy dialog
      _ -> error "??? in fileSaveAs"
    return ()





