module Hoodle.Coroutine.Highlighter where

import Hoodle.Coroutine.Pen (penStart)
import Hoodle.Device (PointerCoord)
import Hoodle.Type.Canvas (CanvasId)
import Hoodle.Type.Coroutine (MainCoroutine)

-- |
highlighterStart ::
  CanvasId ->
  PointerCoord ->
  MainCoroutine (Maybe (Maybe (Maybe ())))
highlighterStart = penStart
