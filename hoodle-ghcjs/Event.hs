module Event where

import Message (Commit)

data AllEvent
  = Fire
  | ERegisterStroke (Int, Int)
  | EDataStrokes [Commit] -- [(Int, [(Double, Double)])]
  | PointerDown (Double, Double)
  | PointerMove (Double, Double)
  | PointerUp (Double, Double)
  | ToPenMode
  | ToEraserMode
  deriving (Show)
