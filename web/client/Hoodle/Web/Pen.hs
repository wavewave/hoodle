module Hoodle.Web.Pen where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get), modify')
import Data.Foldable (toList)
import Data.Hashable (hash)
import qualified Data.JSString as JSS (pack)
import Data.Sequence (Seq, ViewR (..), viewr, (|>))
import qualified Data.Text as T
import Hoodle.Web.Default (nextevent)
import qualified Hoodle.Web.ForeignJS as J
import Hoodle.Web.Type.Coroutine (MainCoroutine)
import Hoodle.Web.Type.Event (UserEvent (..))
import Hoodle.Web.Type.State
  ( hdlstateOverlayCanvas,
    hdlstateOverlayOffCanvas,
    hdlstateOverlayUpdated,
    hdlstateSVGBox,
    hdlstateSyncState,
    hdlstateWebSocket,
    syncstateQueue,
  )
import Hoodle.Web.Util (transformPathFromCanvasToSVG)
import qualified JavaScript.Web.WebSocket as WS
import Lens.Micro ((.~), (^.))
import Message
  ( C2SMsg (NewStroke),
    TextSerializable (serialize),
  )

drawingMode :: Seq (Double, Double) -> MainCoroutine ()
drawingMode cxys = do
  ev <- nextevent
  case ev of
    PointerMove cxy@(cx, cy) -> do
      s <- get
      let cvs = s ^. hdlstateOverlayCanvas
          offcvs = s ^. hdlstateOverlayOffCanvas
      case viewr cxys of
        _ :> (cx0, cy0) -> liftIO $ J.js_overlay_point cvs offcvs cx0 cy0 cx cy
        _ -> pure ()
      modify' (hdlstateOverlayUpdated .~ True)
      drawingMode (cxys |> cxy)
    PointerUp cxy -> do
      s <- get
      let svg = s ^. hdlstateSVGBox
          sock = s ^. hdlstateWebSocket
      let cxys' = cxys |> cxy
      path <- liftIO $ transformPathFromCanvasToSVG svg (toList cxys')
      modify'
        ( (hdlstateSyncState . syncstateQueue .~ [path])
            . (hdlstateOverlayUpdated .~ True)
        )
      let hsh = hash path
          msg = NewStroke (hsh, path)
      liftIO $ WS.send (JSS.pack . T.unpack . serialize $ msg) sock
    _ -> drawingMode cxys
