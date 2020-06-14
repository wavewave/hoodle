module Hoodle.Coroutine.Highlighter where

import Hoodle.Coroutine.Pen
import Hoodle.Device
import Hoodle.Type.Canvas
import Hoodle.Type.Coroutine

-- |
highlighterStart ::
  CanvasId ->
  PointerCoord ->
  MainCoroutine (Maybe (Maybe (Maybe ())))
highlighterStart cid pcoord = penStart cid pcoord
