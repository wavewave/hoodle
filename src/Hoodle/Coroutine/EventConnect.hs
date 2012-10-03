-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.EventConnect 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.EventConnect where

import Graphics.UI.Gtk hiding (get,set,disconnect)
-- import qualified Control.Monad.State as St
import Control.Applicative
import Control.Monad.Trans
import Control.Category
import Control.Lens
import Control.Monad.State 
-- 
import Control.Monad.Trans.Crtn.Event
import Control.Monad.Trans.Crtn.Queue 
-- 
import Hoodle.Type.Event
import Hoodle.Type.Canvas
import Hoodle.Type.HoodleState
import Hoodle.Device
import Hoodle.Type.Coroutine
-- 
import Prelude hiding ((.), id)

-- |

disconnect :: (WidgetClass w) => [ConnectId w] -> MainCoroutine () 
disconnect is = modify (tempQueue %~ enqueue action) >> go 
  where 
    go = do r <- nextevent 
            case r of
              EventDisconnected -> return ()
              _ -> go 
    action = Left . ActionOrder $ 
      \evhandler -> mapM_ signalDisconnect is >> return EventDisconnected
--   liftIO . signalDisconnect

-- |

connectPenUp :: CanvasInfo a -> MainCoroutine (ConnectId DrawingArea) 
connectPenUp cinfo = do 
  let cid = view canvasId cinfo
      canvas = view drawArea cinfo 
  connPenUp canvas cid 

-- |

connectPenMove :: CanvasInfo a -> MainCoroutine (ConnectId DrawingArea) 
connectPenMove cinfo = do 
  let cid = view canvasId cinfo
      canvas = view drawArea cinfo 
  connPenMove canvas cid 

-- |

connPenMove :: (WidgetClass w) => w -> CanvasId -> MainCoroutine (ConnectId w) 
connPenMove c cid = do 
  callbk <- view callBack <$> get
  dev <- view deviceList <$> get
  liftIO (c `on` motionNotifyEvent $ tryEvent $ do 
             (_,p) <- getPointer dev
             liftIO (callbk (PenMove cid p)))

-- | 
  
connPenUp :: (WidgetClass w) => w -> CanvasId -> MainCoroutine (ConnectId w)
connPenUp c cid = do 
  callbk <- view callBack <$> get
  dev <- view deviceList <$> get
  liftIO (c `on` buttonReleaseEvent $ tryEvent $ do 
             (_,p) <- getPointer dev
             liftIO (callbk (PenMove cid p)))
