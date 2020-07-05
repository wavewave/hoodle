{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Handler where

import Control.Monad (when)
import Control.Monad.Trans.Crtn.EventHandler (eventHandler)
import Coroutine (EventVar)
import qualified Data.JSString as JSS (unpack)
import qualified Data.Text as T
import qualified ForeignJS as J
import GHCJS.Foreign.Callback
  ( Callback,
    OnBlocked (ThrowWouldBlock),
    syncCallback,
    syncCallback1,
  )
import GHCJS.Types (JSString, JSVal, jsval)
import Hoodle.Web.Type.Event (AllEvent (..), SystemEvent (..), UserEvent (..))
import Hoodle.Web.Util (putStrLnAndFlush)
import qualified JavaScript.Web.MessageEvent as ME
import qualified JavaScript.Web.WebSocket as WS
import Message
  ( CommitId (..),
    S2CMsg (DataStrokes, RegisterStroke),
    TextSerializable (deserialize),
  )
import State (DocState (..), HoodleState (..), SyncState (..))

onPointerDown ::
  EventVar ->
  JSVal ->
  IO ()
onPointerDown evar ev = do
  v <- J.js_pointer_type ev
  J.js_debug_show (jsval v)
  t <- J.getPointerType ev
  when (t /= J.Touch) $ do
    (x, y) <- J.getXY ev
    eventHandler evar $ UsrEv $ PointerDown (x, y)

onPointerUp ::
  EventVar ->
  JSVal ->
  IO ()
onPointerUp evar ev = do
  J.js_debug_show $ jsval ("ready for input" :: JSString)
  t <- J.getPointerType ev
  when (t /= J.Touch) $ do
    (x, y) <- J.getXY ev
    eventHandler evar $ UsrEv $ PointerUp (x, y)

onPointerMove ::
  EventVar ->
  JSVal ->
  IO ()
onPointerMove evar ev = do
  t <- J.getPointerType ev
  when (t /= J.Touch) $ do
    (x, y) <- J.getXY ev
    eventHandler evar $ UsrEv $ PointerMove (x, y)

onAnimationFrame :: EventVar -> Callback (IO ()) -> IO ()
onAnimationFrame evar rAF = do
  eventHandler evar $ SysEv $ ERefresh
  J.js_requestAnimationFrame rAF

onMessage :: EventVar -> JSString -> IO ()
onMessage evar s = do
  let str = JSS.unpack s
      txt = T.pack str
  case deserialize txt of
    RegisterStroke s' -> do
      eventHandler evar $ SysEv $ ERegisterStroke s'
    DataStrokes dat -> do
      eventHandler evar $ SysEv $ EDataStrokes dat

data Mode = ModePen | ModeEraser | ModeSelect
  deriving (Show)

data ModeButtons
  = ModeButtons
      { _mbPen :: JSVal,
        _mbEraser :: JSVal,
        _mbSelect :: JSVal
      }

onModeChange :: Mode -> EventVar -> ModeButtons -> JSVal -> IO ()
onModeChange m evar btns _ = do
  case m of
    ModePen -> do
      J.js_add_class (_mbPen btns) "is-primary"
      J.js_remove_class (_mbEraser btns) "is-primary"
      J.js_remove_class (_mbSelect btns) "is-primary"
      eventHandler evar $ UsrEv ToPenMode
    ModeEraser -> do
      J.js_remove_class (_mbPen btns) "is-primary"
      J.js_add_class (_mbEraser btns) "is-primary"
      J.js_remove_class (_mbSelect btns) "is-primary"
      eventHandler evar $ UsrEv ToEraserMode
    ModeSelect -> do
      J.js_remove_class (_mbPen btns) "is-primary"
      J.js_remove_class (_mbEraser btns) "is-primary"
      J.js_add_class (_mbSelect btns) "is-primary"
      eventHandler evar $ UsrEv ToSelectMode

setupCallback :: EventVar -> IO HoodleState
setupCallback evar = do
  putStrLn "ghcjs started"
  J.js_prevent_default_touch_move
  svg <- J.js_svg_box
  cvs <- J.js_document_getElementById "overlay"
  J.js_fix_dpi cvs
  offcvs <- J.js_create_canvas
  w <- J.js_get_width cvs
  h <- J.js_get_height cvs
  J.js_set_width offcvs w
  J.js_set_height offcvs h
  putStrLn "websocket start"
  let wsClose _ =
        putStrLnAndFlush "connection closed"
      wsMessage ev msg = do
        let d = ME.getData msg
        case d of
          ME.StringData s -> onMessage ev s
          _ -> pure ()
  xstate <- mdo
    sock <-
      WS.connect
        WS.WebSocketRequest
          { WS.url = "ws://192.168.1.42:7080",
            WS.protocols = [],
            WS.onClose = Just wsClose,
            WS.onMessage = Just (wsMessage evar)
          }
    pure $ HoodleState svg cvs offcvs sock (DocState 0 []) (SyncState []) True
  onpointerdown <- syncCallback1 ThrowWouldBlock (onPointerDown evar)
  J.js_addEventListener cvs "pointerdown" onpointerdown
  onpointermove <- syncCallback1 ThrowWouldBlock (onPointerMove evar)
  J.js_addEventListener cvs "pointermove" onpointermove
  onpointerup <- syncCallback1 ThrowWouldBlock (onPointerUp evar)
  J.js_addEventListener cvs "pointerup" onpointerup
  mdo
    rAF <- syncCallback ThrowWouldBlock (onAnimationFrame evar rAF)
    J.js_requestAnimationFrame rAF
  pen <- J.js_document_getElementById "pen"
  eraser <- J.js_document_getElementById "eraser"
  select <- J.js_document_getElementById "select"
  let btns = ModeButtons pen eraser select
  J.js_addEventListener pen "click"
    =<< syncCallback1 ThrowWouldBlock (onModeChange ModePen evar btns)
  J.js_addEventListener eraser "click"
    =<< syncCallback1 ThrowWouldBlock (onModeChange ModeEraser evar btns)
  J.js_addEventListener select "click"
    =<< syncCallback1 ThrowWouldBlock (onModeChange ModeSelect evar btns)
  pure xstate
