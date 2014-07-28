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

import Control.Lens ((%~))
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
      let ncinfobox = 
            ( (unboxLens (canvasWidgets.widgetConfig.doesUseScrollWidget) %~ not)
            . getCanvasInfo cid )  xst 
      in setCanvasInfo (cid,ncinfobox) xst
    invalidateInBBox Nothing Efficient cid
