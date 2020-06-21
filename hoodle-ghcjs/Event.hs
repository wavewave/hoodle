module Event where

import Message (Commit)

data AllEvent
  = UsrEv UserEvent
  | SysEv SystemEvent

data SystemEvent
  = ERegisterStroke Int
  | EDataStrokes [Commit]
  | ERefresh

data UserEvent
  = PointerDown (Double, Double)
  | PointerMove (Double, Double)
  | PointerUp (Double, Double)
  | ToPenMode
  | ToEraserMode
  deriving (Show)
