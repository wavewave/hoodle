module Hoodle.Web.Type.Event where

import Message (Commit, CommitId)

data AllEvent
  = UsrEv UserEvent
  | SysEv SystemEvent

data SystemEvent
  = ERegisterStroke CommitId
  | EDataStrokes [Commit]
  | ERefresh

data UserEvent
  = PointerDown (Double, Double)
  | PointerMove (Double, Double)
  | PointerUp (Double, Double)
  | ToPenMode
  | ToEraserMode
  | ToSelectMode
  deriving (Show)
