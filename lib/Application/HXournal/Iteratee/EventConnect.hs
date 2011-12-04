module Application.HXournal.Iteratee.EventConnect where

import Graphics.UI.Gtk hiding (get,set,disconnect)
import Application.HXournal.Type.Event
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Device
import Application.HXournal.Type.Coroutine

import qualified Control.Monad.State as St
import Control.Applicative
import Control.Monad.Trans

import Control.Category
import Data.Label 
import Prelude hiding ((.), id)

disconnect :: (WidgetClass w) => ConnectId w 
              -> Iteratee MyEvent XournalStateIO ()
disconnect = liftIO . signalDisconnect

connectPenUp :: CanvasInfo -> Iteratee MyEvent XournalStateIO (ConnectId DrawingArea)
connectPenUp cinfo = do 
  let cid = get canvasId cinfo
      canvas = get drawArea cinfo 
  connPenUp canvas cid 

connectPenMove :: CanvasInfo -> Iteratee MyEvent XournalStateIO (ConnectId DrawingArea)
connectPenMove cinfo = do 
  let cid = get canvasId cinfo
      canvas = get drawArea cinfo 
  connPenMove canvas cid 

connPenMove :: (WidgetClass w) => 
               w 
               -> CanvasId 
               -> Iteratee MyEvent XournalStateIO (ConnectId w) 
connPenMove c cid = do 
  callbk <- get callBack <$> lift St.get 
  dev <- get deviceList <$> lift St.get 
  liftIO (c `on` motionNotifyEvent $ tryEvent $ do 
             p <- getPointer dev
             liftIO (callbk (PenMove cid p)))

connPenUp :: (WidgetClass w) => 
             w 
             -> CanvasId
             -> Iteratee MyEvent XournalStateIO (ConnectId w) 
connPenUp c cid = do 
  callbk <- get callBack <$> lift St.get 
  dev <- get deviceList <$> lift St.get 
  liftIO (c `on` buttonReleaseEvent $ tryEvent $ do 
             p <- getPointer dev
             liftIO (callbk (PenMove cid p)))
