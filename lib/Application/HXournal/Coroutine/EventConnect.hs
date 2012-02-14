-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Coroutine.EventConnect 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.Coroutine.EventConnect where

import Graphics.UI.Gtk hiding (get,set,disconnect)
import Application.HXournal.Type.Event
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Device
import Application.HXournal.Type.Coroutine
import Application.HXournal.Accessor

-- import qualified Control.Monad.State as St
import Control.Applicative
import Control.Monad.Trans

import Control.Category
import Data.Label 
import Prelude hiding ((.), id)

-- |

disconnect :: (WidgetClass w) => ConnectId w -> MainCoroutine () 
disconnect = liftIO . signalDisconnect

-- |

connectPenUp :: CanvasInfo a -> MainCoroutine (ConnectId DrawingArea) 
connectPenUp cinfo = do 
  let cid = get canvasId cinfo
      canvas = get drawArea cinfo 
  connPenUp canvas cid 

-- |

connectPenMove :: CanvasInfo a -> MainCoroutine (ConnectId DrawingArea) 
connectPenMove cinfo = do 
  let cid = get canvasId cinfo
      canvas = get drawArea cinfo 
  connPenMove canvas cid 

-- |

connPenMove :: (WidgetClass w) => w -> CanvasId -> MainCoroutine (ConnectId w) 
connPenMove c cid = do 
  callbk <- get callBack <$> getSt
  dev <- get deviceList <$> getSt
  liftIO (c `on` motionNotifyEvent $ tryEvent $ do 
             (_,p) <- getPointer dev
             liftIO (callbk (PenMove cid p)))

-- | 
  
connPenUp :: (WidgetClass w) => w -> CanvasId -> MainCoroutine (ConnectId w)
connPenUp c cid = do 
  callbk <- get callBack <$> getSt
  dev <- get deviceList <$> getSt
  liftIO (c `on` buttonReleaseEvent $ tryEvent $ do 
             (_,p) <- getPointer dev
             liftIO (callbk (PenMove cid p)))
