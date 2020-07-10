module Hoodle.Web.Type.State where

import GHCJS.Types (JSVal)
import Hoodle.HitTest.Type (BBoxed)
import qualified JavaScript.Web.WebSocket as WS
import Message (CommitId (..))

data SyncState
  = SyncState
      { _syncstateQueue :: [[(Double, Double)]]
      }

data RStroke
  = RStroke
      { rstrokeCommitId :: CommitId,
        rstrokePath :: [(Double, Double)]
      }

data DocState
  = DocState
      { _docstateLastCommit :: CommitId,
        _docstateData :: [BBoxed RStroke]
      }

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
