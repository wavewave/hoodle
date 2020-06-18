{-# LANGUAGE GADTs #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get, put), modify')
import qualified Control.Monad.Trans.Crtn.Driver as D (driver)
import Control.Monad.Trans.Crtn.EventHandler (eventHandler)
import Control.Monad.Trans.Crtn.Object (Arg (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Coroutine
  ( EventVar,
    MainCoroutine,
    MainObj,
    MainOp (DoEvent),
    nextevent,
    putStrLnAndFlush,
    simplelogger,
    world,
  )
import Data.Foldable (toList)
import Data.Hashable (hash)
import qualified Data.JSString as JSS (pack, unpack)
import Data.Sequence (Seq, ViewR (..), singleton, viewr, (|>))
import qualified Data.Text as T
import Event (AllEvent (..))
import GHCJS.Foreign.Callback
  ( Callback,
    OnBlocked (ThrowWouldBlock),
    syncCallback,
    syncCallback1,
  )
import GHCJS.Marshal (FromJSVal (..), ToJSVal (..))
import GHCJS.Types (JSString, JSVal, jsval)
import qualified JavaScript.Web.MessageEvent as ME
import qualified JavaScript.Web.WebSocket as WS
import Message
  ( C2SMsg (NewStroke, SyncRequest),
    S2CMsg (DataStrokes, RegisterStroke),
    TextSerializable (deserialize, serialize),
  )
import State (DocState (..), HoodleState (..), SyncState (..))

foreign import javascript unsafe "console.log($1)"
  js_console_log :: JSVal -> IO ()

foreign import javascript unsafe "preventDefaultTouchMove()"
  js_prevent_default_touch_move :: IO ()

foreign import javascript unsafe "$r = SVG('#box')"
  js_svg_box :: IO JSVal

-- foreign import javascript unsafe "$r = document.getElementById('overlay')"
--  js_canvas_overlay :: IO JSVal

foreign import javascript unsafe "$1.on($2,$3)"
  js_on :: JSVal -> JSString -> Callback a -> IO ()

foreign import javascript unsafe "$r = $1.clientX"
  js_clientX :: JSVal -> IO Double

foreign import javascript unsafe "$r = $1.clientY"
  js_clientY :: JSVal -> IO Double

foreign import javascript unsafe "$r = toSVGPointArray($1,$2)"
  js_to_svg_point_array :: JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe "drawPath($1,$2)"
  js_draw_path :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "window.requestAnimationFrame($1)"
  js_requestAnimationFrame :: Callback a -> IO ()

foreign import javascript unsafe "refresh($1,$2)"
  js_refresh :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.addEventListener($2,$3)"
  js_addEventListener :: JSVal -> JSString -> Callback a -> IO ()

foreign import javascript unsafe "overlay_point($1,$2,$3,$4,$5,$6)"
  js_overlay_point :: JSVal -> JSVal -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "clear_overlay($1)"
  js_clear_overlay :: JSVal -> IO ()

foreign import javascript unsafe "fix_dpi($1)"
  js_fix_dpi :: JSVal -> IO ()

foreign import javascript unsafe "$r = $1.width"
  js_get_width :: JSVal -> IO Double

foreign import javascript unsafe "$1.width = $2"
  js_set_width :: JSVal -> Double -> IO ()

foreign import javascript unsafe "$r = $1.height"
  js_get_height :: JSVal -> IO Double

foreign import javascript unsafe "$1.height = $2"
  js_set_height :: JSVal -> Double -> IO ()

foreign import javascript unsafe "$r = document.createElement('canvas')"
  js_create_canvas :: IO JSVal

foreign import javascript unsafe "$r = $1.pointerType"
  js_pointer_type :: JSVal -> IO JSString

foreign import javascript unsafe "debug_show($1)"
  js_debug_show :: JSVal -> IO ()

foreign import javascript unsafe "document.getElementById($1)"
  js_document_getElementById :: JSString -> IO JSVal

data PointerType = Mouse | Touch | Pen
  deriving (Show, Eq)

getPointerType :: JSVal -> IO PointerType
getPointerType ev = js_pointer_type ev >>= \s -> do
  case JSS.unpack s of
    "touch" -> pure Touch
    "pen" -> pure Pen
    _ -> pure Mouse

getXY :: JSVal -> IO (Double, Double)
getXY ev = (,) <$> js_clientX ev <*> js_clientY ev

drawPath :: JSVal -> [(Double, Double)] -> IO ()
drawPath svg xys = do
  arr <- toJSValListOf xys
  js_draw_path svg arr

onPointerDown ::
  EventVar ->
  JSVal ->
  IO ()
onPointerDown evar ev = do
  v <- js_pointer_type ev
  js_debug_show (jsval v)
  t <- getPointerType ev
  when (t /= Touch) $ do
    (x, y) <- getXY ev
    eventHandler evar $ PointerDown (x, y)

onPointerUp ::
  EventVar ->
  JSVal ->
  IO ()
onPointerUp evar ev = do
  js_debug_show $ jsval ("ready for input" :: JSString)
  t <- getPointerType ev
  when (t /= Touch) $ do
    (x, y) <- getXY ev
    eventHandler evar (PointerUp (x, y))

onPointerMove ::
  EventVar ->
  JSVal ->
  IO ()
onPointerMove evar ev = do
  t <- getPointerType ev
  when (t /= Touch) $ do
    (x, y) <- getXY ev
    eventHandler evar (PointerMove (x, y))

test :: JSVal -> JSVal -> Callback (IO ()) -> IO ()
test cvs offcvs rAF = do
  js_refresh cvs offcvs
  js_requestAnimationFrame rAF

onMessage :: EventVar -> JSString -> IO ()
onMessage evar s = do
  case deserialize $ T.pack $ JSS.unpack s of
    RegisterStroke (s', hsh') -> do
      eventHandler evar (ERegisterStroke (s', hsh'))
    DataStrokes dat -> do
      eventHandler evar (EDataStrokes dat)

data Mode = ModePen | ModeEraser
  deriving (Show)

onModeChange :: Mode -> EventVar -> JSVal -> IO ()
onModeChange m evar _ = do
  case m of
    ModePen -> eventHandler evar ToPenMode
    ModeEraser -> eventHandler evar ToEraserMode

guiProcess :: AllEvent -> MainCoroutine ()
guiProcess = toPenMode

toPenMode :: AllEvent -> MainCoroutine ()
toPenMode ev = do
  case ev of
    ERegisterStroke (s', hsh') -> do
      HoodleState _ _ _ sock (DocState n) _ <- get
      liftIO $ putStrLnAndFlush (show s' ++ " <-> " ++ show n)
      liftIO $ putStrLnAndFlush (show hsh')
      when (s' > n) $ liftIO $ do
        let msg = SyncRequest (n, s')
        WS.send (JSS.pack . T.unpack . serialize $ msg) sock
    EDataStrokes dat -> do
      st@(HoodleState svg _ offcvs _ _ _) <- get
      liftIO $ do
        js_clear_overlay offcvs
        mapM_ (drawPath svg . snd) dat
      let i = maximum (map fst dat)
      put $ st {_hdlstateDocState = DocState i}
    PointerDown (x, y) ->
      drawingMode (singleton (x, y))
    ToPenMode -> pure ()
    ToEraserMode -> nextevent >>= toEraseMode
    _ -> do
      liftIO $ putStrLnAndFlush (show ev)
  nextevent >>= toPenMode

toEraseMode :: AllEvent -> MainCoroutine ()
toEraseMode ev = do
  case ev of
    ToPenMode -> nextevent >>= toPenMode
    _ -> nextevent >>= toEraseMode

drawingMode :: Seq (Double, Double) -> MainCoroutine ()
drawingMode xys = do
  ev <- nextevent
  case ev of
    PointerMove xy@(x, y) -> do
      HoodleState _svg cvs offcvs _ _ _ <- get
      case viewr xys of
        _ :> (x0, y0) -> liftIO $ js_overlay_point cvs offcvs x0 y0 x y
        _ -> pure ()
      drawingMode (xys |> xy)
    PointerUp xy -> do
      HoodleState svg _ _offcvs sock _ _ <- get
      let xys' = xys |> xy
      path_arr <-
        liftIO $
          js_to_svg_point_array svg =<< toJSValListOf (toList xys')
      path <- liftIO $ fromJSValUncheckedListOf path_arr
      modify' (\s -> s {_hdlstateSyncState = SyncState [path]})
      let hsh = hash path
          msg = NewStroke (hsh, path)
      liftIO $ WS.send (JSS.pack . T.unpack . serialize $ msg) sock
    _ -> drawingMode xys

initmc :: MainObj ()
initmc = ReaderT $ (\(Arg DoEvent ev) -> guiProcess ev)

setupCallback :: EventVar -> IO HoodleState
setupCallback evar = do
  putStrLn "ghcjs started"
  js_prevent_default_touch_move
  svg <- js_svg_box
  cvs <- js_document_getElementById "overlay"
  js_fix_dpi cvs
  offcvs <- js_create_canvas
  w <- js_get_width cvs
  h <- js_get_height cvs
  js_set_width offcvs w
  js_set_height offcvs h
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
    pure $ HoodleState svg cvs offcvs sock (DocState 0) (SyncState [])
  onpointerdown <- syncCallback1 ThrowWouldBlock (onPointerDown evar)
  js_addEventListener cvs "pointerdown" onpointerdown
  onpointermove <- syncCallback1 ThrowWouldBlock (onPointerMove evar)
  js_addEventListener cvs "pointermove" onpointermove
  onpointerup <- syncCallback1 ThrowWouldBlock (onPointerUp evar)
  js_addEventListener cvs "pointerup" onpointerup
  mdo
    rAF <- syncCallback ThrowWouldBlock (test cvs offcvs rAF)
    js_requestAnimationFrame rAF
  radio_pen <- js_document_getElementById "pen"
  radio_eraser <- js_document_getElementById "eraser"
  js_addEventListener radio_pen "click" =<< syncCallback1 ThrowWouldBlock (onModeChange ModePen evar)
  js_addEventListener radio_eraser "click" =<< syncCallback1 ThrowWouldBlock (onModeChange ModeEraser evar)
  pure xstate

main :: IO ()
main = do
  putStrLn "new start"
  evar <- newEmptyMVar :: IO EventVar
  xstate <- setupCallback evar
  putMVar evar . Just $ D.driver simplelogger (world xstate initmc)
