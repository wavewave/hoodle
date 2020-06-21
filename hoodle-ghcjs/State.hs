module State where

import GHCJS.Types (JSVal)
import qualified JavaScript.Web.WebSocket as WS

data SyncState
  = SyncState
      { _syncstateQueue :: [[(Double, Double)]]
      }

data DocState
  = DocState
      { _docstateCount :: Int,
        _docstateData :: [(Int, [(Double, Double)])]
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
