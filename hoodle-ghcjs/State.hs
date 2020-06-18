module State where

import GHCJS.Types (JSVal)
import qualified JavaScript.Web.WebSocket as WS

data SyncState
  = SyncState
      { _syncstateQueue :: [[(Double, Double)]]
      }

data DocState
  = DocState
      { _docstateCount :: Int
      }

data HoodleState
  = HoodleState
      { _hdlstateSVGBox :: JSVal,
        _hdlstateOverlayCanvas :: JSVal,
        _hdlstateOverlayOffCanvas :: JSVal,
        _hdlstateWebSocket :: WS.WebSocket,
        _hdlstateDocState :: DocState,
        _hdlstateSyncState :: SyncState
      }
