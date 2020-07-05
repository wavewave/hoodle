module Hoodle.Web.Pen where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get, put), modify')
import Coroutine (MainCoroutine)
import Data.Foldable (toList)
import Data.Hashable (hash)
import qualified Data.JSString as JSS (pack)
import Data.Sequence (Seq, ViewR (..), viewr, (|>))
import qualified Data.Text as T
import Event (UserEvent (..))
import qualified ForeignJS as J
import Hoodle.Web.Default (nextevent)
import Hoodle.Web.Util (transformPathFromCanvasToSVG)
import qualified JavaScript.Web.WebSocket as WS
import Message
  ( C2SMsg (NewStroke),
    TextSerializable (serialize),
  )
import State (HoodleState (..), SyncState (..))

drawingMode :: Seq (Double, Double) -> MainCoroutine ()
drawingMode cxys = do
  ev <- nextevent
  case ev of
    PointerMove cxy@(cx, cy) -> do
      s@(HoodleState _ cvs offcvs _ _ _ _) <- get
      case viewr cxys of
        _ :> (cx0, cy0) -> liftIO $ J.js_overlay_point cvs offcvs cx0 cy0 cx cy
        _ -> pure ()
      put $ s {_hdlstateOverlayUpdated = True}
      drawingMode (cxys |> cxy)
    PointerUp cxy -> do
      HoodleState svg _ _ sock _ _ _ <- get
      let cxys' = cxys |> cxy
      path <- liftIO $ transformPathFromCanvasToSVG svg (toList cxys')
      modify'
        ( \s ->
            s
              { _hdlstateSyncState = SyncState [path],
                _hdlstateOverlayUpdated = True
              }
        )
      let hsh = hash path
          msg = NewStroke (hsh, path)
      liftIO $ WS.send (JSS.pack . T.unpack . serialize $ msg) sock
    _ -> drawingMode cxys
