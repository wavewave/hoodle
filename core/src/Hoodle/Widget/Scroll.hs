-- |
-- Module      : Hoodle.Widget.Scroll
-- Copyright   : (c) 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Pan-Zoom widget drawing and action
module Hoodle.Widget.Scroll where

import Control.Lens ((%~))
import Hoodle.Accessor (pureUpdateUhdl)
import Hoodle.Coroutine.Draw (invalidateInBBox)
import Hoodle.Type.Canvas
  ( CanvasId,
    canvasWidgets,
    unboxLens,
  )
import Hoodle.Type.Coroutine (MainCoroutine)
import Hoodle.Type.Enum (DrawFlag (Efficient))
import Hoodle.Type.HoodleState
  ( getCanvasInfo,
    setCanvasInfo,
  )
import Hoodle.Type.Widget
  ( doesUseScrollWidget,
    widgetConfig,
  )

-- |
toggleScroll :: CanvasId -> MainCoroutine ()
toggleScroll cid = do
  pureUpdateUhdl $ \uhdl ->
    let ncinfobox =
          ( (unboxLens (canvasWidgets . widgetConfig . doesUseScrollWidget) %~ not)
              . getCanvasInfo cid
          )
            uhdl
     in setCanvasInfo (cid, ncinfobox) uhdl
  invalidateInBBox Nothing Efficient cid
