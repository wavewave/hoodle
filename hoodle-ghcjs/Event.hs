module Event where

data AllEvent
  = Fire
  | ERegisterStroke (Int, Int)
  | EDataStrokes [(Int, [(Double, Double)])]
  | PointerDown (Double, Double)
  | PointerMove (Double, Double)
  | PointerUp (Double, Double)
  | ToPenMode
  | ToEraserMode
  deriving (Show)
