-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Widget.Scroll
-- Copyright   : (c) 2014 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Pan-Zoom widget drawing and action
-- 
-----------------------------------------------------------------------------

module Hoodle.Widget.Scroll where

import Control.Lens (set, view, (%~))
import Control.Monad.State
--
import Hoodle.Coroutine.Draw
import Hoodle.Type.Canvas
import Hoodle.Type.Coroutine
import Hoodle.Type.Enum
import Hoodle.Type.HoodleState
import Hoodle.Type.Widget


-- | 
toggleScroll :: CanvasId -> MainCoroutine () 
toggleScroll cid = do 
    modify $ \xst -> 
      let uhdl = (getTheUnit . view unitHoodles) xst
          ncinfobox = 
            ( (unboxLens (canvasWidgets.widgetConfig.doesUseScrollWidget) %~ not)
            . getCanvasInfo cid ) uhdl 
      in set unitHoodles (putTheUnit (setCanvasInfo (cid,ncinfobox) uhdl)) xst
    invalidateInBBox Nothing Efficient cid
