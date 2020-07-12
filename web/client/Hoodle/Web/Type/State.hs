{-# LANGUAGE TemplateHaskell #-}

module Hoodle.Web.Type.State where

import GHCJS.Types (JSVal)
import Hoodle.HitTest.Type (BBoxed)
import qualified JavaScript.Web.WebSocket as WS
import Lens.Micro.TH (makeLenses)
import Message (CommitId (..))

data SyncState
  = SyncState
      { _syncstateQueue :: [[(Double, Double)]]
      }

makeLenses ''SyncState

data RStroke
  = RStroke
      { _rstrokeCommitId :: CommitId,
        _rstrokePath :: [(Double, Double)]
      }

makeLenses ''RStroke

data DocState
  = DocState
      { _docstateLastCommit :: CommitId,
        _docstateData :: [BBoxed RStroke]
      }

makeLenses ''DocState

data HoodleState
  = HoodleState
      { _hdlstateSVGBox :: JSVal,
        _hdlstateOverlayCanvas :: JSVal,
        _hdlstateOverlayOffCanvas :: JSVal,
        _hdlstateWebSocket :: WS.WebSocket,
        _hdlstateDocState :: DocState,
        _hdlstateSyncState :: SyncState,
        _hdlstateOverlayUpdated :: Bool
      }

makeLenses ''HoodleState
