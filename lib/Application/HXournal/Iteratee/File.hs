module Application.HXournal.Iteratee.File where

import Application.HXournal.Type.Event
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.XournalState
import Application.HXournal.Accessor
import Application.HXournal.ModelAction.File 
import Application.HXournal.Iteratee.Draw
import Application.HXournal.Builder 

import Control.Monad.Trans
import Control.Applicative

import Graphics.Xournal.Type.Map
import Graphics.Xournal.Type.Select
import Graphics.UI.Gtk hiding (get,set)

import Control.Category
import Data.Label
import Prelude hiding ((.),id)

import qualified Data.ByteString.Lazy as L

fileSave :: Iteratee MyEvent XournalStateIO () 
fileSave = do 
    xstate <- getSt 
    let xojstate = get xournalstate xstate
        filename = case get currFileName xstate of
                     Nothing -> "mytest.xoj"
                     Just fn -> fn
    let xoj = case xojstate of 
                 ViewAppendState xojmap -> xournalFromXournalBBoxMap xojmap 
                 SelectState txoj -> xournalFromXournalBBoxMap 
                                     $ XournalBBoxMap <$> tx_pages $ txoj 
    liftIO . L.writeFile filename . builder $ xoj

fileOpen :: Iteratee MyEvent XournalStateIO ()
fileOpen = do 
    liftIO $ putStrLn "file open clicked"
    dialog <- liftIO $ fileChooserDialogNew Nothing Nothing FileChooserActionOpen 
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
            let {- devlst = get deviceList xstate
                cinfo1 = getCanvasInfo 1 xstate
                cinfo2 = getCanvasInfo 2 xstate
                canvas = get drawArea cinfo1
                canvas2 = get drawArea cinfo2
                (hadj,vadj) = get adjustments cinfo1
                (hadj2,vadj2) = get adjustments cinfo2
                callbackfunc = get callBack xstate  -}
            xstateNew <- liftIO $ getFileContent filename xstate
{-                                    devlst 
                                    (canvas,hadj,vadj) 
                                    (canvas2,hadj2,vadj2) 
                                    callbackfunc 
                                    filename -}
            putSt xstateNew 
            invalidateAll 
        liftIO $ widgetDestroy dialog
      ResponseCancel -> liftIO $ widgetDestroy dialog
    return ()

fileSaveAs :: Iteratee MyEvent XournalStateIO ()
fileSaveAs = do 
    liftIO $ putStrLn "file save as clicked"
    dialog <- liftIO $ fileChooserDialogNew Nothing Nothing FileChooserActionSave 
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
            putSt xstate                                     
            fileSave
        liftIO $ widgetDestroy dialog
      ResponseCancel -> liftIO $ widgetDestroy dialog
    return ()

