-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Widget.Clock
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Clock widget drawing and action
-- 
-----------------------------------------------------------------------------

module Hoodle.Widget.Clock where

import           Control.Lens (view,set,over)
import Control.Monad.State 
import Control.Monad.Trans
import           Data.List (delete)
import           Data.Sequence
import           Data.Time
import           Graphics.Rendering.Cairo
--
import           Data.Hoodle.BBox
import           Data.Hoodle.Simple 
import           Graphics.Hoodle.Render.Util.HitTest
--
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Layer
import           Hoodle.Coroutine.Pen
import           Hoodle.Device
import           Hoodle.ModelAction.Select
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState 
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.Widget
import           Hoodle.View.Coordinate
import           Hoodle.View.Draw

-- | 
toggleClock :: CanvasId -> MainCoroutine () 
toggleClock cid = do 
  modify (\xst -> 
            let ncinfobox = (over (unboxLens (canvasWidgets.widgetConfig.doesUseClockWidget)) not 
                             . getCanvasInfo cid ) xst 
            in setCanvasInfo (cid,ncinfobox) xst)
  invalidateInBBox Nothing Efficient cid

