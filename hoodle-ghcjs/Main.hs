{-# LANGUAGE GADTs #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    modifyTVar',
    newTVarIO,
    readTVar,
    writeTVar,
  )
import Control.Monad (forever, liftM, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get, put), modify')
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Crtn ((<==|), CrtnT, request)
import qualified Control.Monad.Trans.Crtn.Driver as D
  ( Driver,
    DrvOp,
    driver,
  )
import Control.Monad.Trans.Crtn.EventHandler (eventHandler)
import Control.Monad.Trans.Crtn.Logger
  ( LogInput,
    LogOp (..),
    LogServer,
    MonadLog (..),
    writeLog,
  )
import Control.Monad.Trans.Crtn.Object (Arg (..), CObjT, EStT, Res (..), SObjBT, SObjT)
import Control.Monad.Trans.Crtn.World (WorldOp (..))
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State (StateT (..))
import Data.Foldable (toList)
import Data.Hashable (hash)
import qualified Data.JSString as JSS (pack, unpack)
import Data.Sequence (Seq, ViewR (..), singleton, viewr, (|>))
import qualified Data.Text as T
import GHCJS.Foreign.Callback
  ( Callback,
    OnBlocked (ContinueAsync, ThrowWouldBlock),
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
import System.IO (hFlush, hPutStrLn, stdout)

foreign import javascript unsafe "console.log($1)"
  js_console_log :: JSVal -> IO ()

foreign import javascript unsafe "preventDefaultTouchMove()"
  js_prevent_default_touch_move :: IO ()

foreign import javascript unsafe "$r = SVG('#box')"
  js_svg_box :: IO JSVal

foreign import javascript unsafe "$r = document.getElementById('overlay')"
  js_canvas_overlay :: IO JSVal

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

data PointerType = Mouse | Touch | Pen
  deriving (Show, Eq)

getPointerType :: JSVal -> IO PointerType
getPointerType ev = js_pointer_type ev >>= \s -> do
  case JSS.unpack s of
    "touch" -> pure Touch
    "pen" -> pure Pen
    _ -> pure Mouse

data DrawingState
  = NotDrawing
  | Drawing (Seq (Double, Double))

data SyncState
  = SyncState
      { _syncstateQueue :: [[(Double, Double)]]
      }

data DocState
  = DocState
      { _docstateCount :: Int
      }

getXY :: JSVal -> IO (Double, Double)
getXY ev = (,) <$> js_clientX ev <*> js_clientY ev

putStrLnAndFlush :: String -> IO ()
putStrLnAndFlush s = do
  hPutStrLn stdout s
  hFlush stdout

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

onMessage :: WS.WebSocket -> EventVar -> JSString -> IO ()
onMessage sock evar s = do
  case deserialize $ T.pack $ JSS.unpack s of
    RegisterStroke (s', hsh') -> do
      eventHandler evar (ERegisterStroke (s', hsh'))
    DataStrokes dat -> do
      eventHandler evar (EDataStrokes dat)

data HoodleState
  = HoodleState
      { _hdlstateIsDrawing :: DrawingState,
        _hdlstateSVGBox :: JSVal,
        _hdlstateOverlayCanvas :: JSVal,
        _hdlstateOverlayOffCanvas :: JSVal,
        _hdlstateWebSocket :: WS.WebSocket,
        _hdlstateDocState :: DocState,
        _hdlstateSyncState :: SyncState
      }

data AllEvent
  = Fire
  | ERegisterStroke (Int, Int)
  | EDataStrokes [(Int, [(Double, Double)])]
  | PointerDown (Double, Double)
  | PointerMove (Double, Double)
  | PointerUp (Double, Double)
  deriving (Show)

data MainOp i o where
  DoEvent :: MainOp AllEvent ()

doEvent :: (Monad m) => AllEvent -> CObjT MainOp m ()
doEvent ev = request (Arg DoEvent ev) >> pure ()

type MainCoroutine = MainObjB

type MainObjB = SObjBT MainOp (EStT HoodleState WorldObjB)

type MainObj = SObjT MainOp (EStT HoodleState WorldObjB)

type WorldObj = SObjT (WorldOp AllEvent DriverB) DriverB

type WorldObjB = SObjBT (WorldOp AllEvent DriverB) DriverB

type Driver a = D.Driver AllEvent IO a

type DriverB = SObjBT (D.DrvOp AllEvent) IO

type EventVar = MVar (Maybe (Driver ()))

simplelogger :: (MonadLog m) => LogServer m ()
simplelogger = loggerW 0

-- |
loggerW :: forall m. (MonadLog m) => Int -> LogServer m ()
loggerW num = ReaderT (f num)
  where
    f :: Int -> LogInput -> CrtnT (Res LogOp) (Arg LogOp) m ()
    f n (Arg WriteLog msg) = do
      lift (scribe ("log number " ++ show n ++ " : " ++ msg))
      req' <- request (Res WriteLog ())
      f (n + 1) req'

errorlog :: String -> IO ()
errorlog = putStrLnAndFlush

nextevent :: MainCoroutine AllEvent
nextevent = do
  Arg DoEvent ev <- request (Res DoEvent ())
  pure ev

-- |
world :: HoodleState -> MainObj () -> WorldObj ()
world xstate initmc = ReaderT staction
  where
    staction req = void $ runStateT erract xstate
      where
        erract = do
          r <- runExceptT (go initmc req)
          case r of
            Left e -> liftIO (errorlog (show e))
            Right _ -> pure ()
    go ::
      MainObj () ->
      Arg (WorldOp AllEvent DriverB) ->
      EStT HoodleState WorldObjB ()
    go mcobj (Arg GiveEvent ev) = do
      Right mcobj' <- liftM (fmap fst) (mcobj <==| doEvent ev)
      req <- lift $ lift $ request $ Res GiveEvent ()
      go mcobj' req
    go mcobj (Arg FlushLog logobj) = do
      Right logobj' <- lift $ lift $ lift $ liftM (fmap fst) (logobj <==| writeLog ("[Log]"))
      req <- lift $ lift $ request $ Res FlushLog logobj'
      go mcobj req
    go mcobj (Arg FlushQueue ()) = do
      req <- lift $ lift $ request $ Res FlushQueue []
      go mcobj req

guiProcess :: AllEvent -> MainCoroutine ()
guiProcess ev = do
  case ev of
    ERegisterStroke (s', hsh') -> do
      HoodleState _ _ _ _ sock (DocState n) _ <- get
      liftIO $ putStrLnAndFlush (show s' ++ " <-> " ++ show n)
      liftIO $ putStrLnAndFlush (show hsh')
      when (s' > n) $ liftIO $ do
        let msg = SyncRequest (n, s')
        WS.send (JSS.pack . T.unpack . serialize $ msg) sock
    EDataStrokes dat -> do
      st@(HoodleState _ svg _ offcvs _ (DocState n) _) <- get
      liftIO $ do
        js_clear_overlay offcvs
        mapM_ (drawPath svg . snd) dat
      let i = maximum (map fst dat)
      put $ st {_hdlstateDocState = DocState i}
    PointerDown (x, y) ->
      drawingMode (singleton (x, y))
    _ -> do
      liftIO $ putStrLnAndFlush (show ev)
  ev <- nextevent
  guiProcess ev

drawingMode :: Seq (Double, Double) -> MainCoroutine ()
drawingMode xys = do
  ev <- nextevent
  case ev of
    PointerMove xy@(x, y) -> do
      HoodleState drawingState svg cvs offcvs _ _ _ <- get
      case viewr xys of
        _ :> (x0, y0) -> liftIO $ js_overlay_point cvs offcvs x0 y0 x y
        _ -> pure ()
      drawingMode (xys |> xy)
    PointerUp xy -> do
      HoodleState drawingState svg _ offcvs sock _ _ <- get
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
  cvs <- js_canvas_overlay
  js_fix_dpi cvs
  offcvs <- js_create_canvas
  w <- js_get_width cvs
  h <- js_get_height cvs
  js_set_width offcvs w
  js_set_height offcvs h
  putStrLn "websocket start"
  let wsClose _ =
        putStrLnAndFlush "connection closed"
      wsMessage sock evar msg = do
        let d = ME.getData msg
        case d of
          ME.StringData s -> onMessage sock evar s
          _ -> pure ()
  xstate <- mdo
    sock <-
      WS.connect
        WS.WebSocketRequest
          { WS.url = "ws://192.168.1.42:7080",
            WS.protocols = [],
            WS.onClose = Just wsClose,
            WS.onMessage = Just (wsMessage sock evar)
          }
    pure $ HoodleState NotDrawing svg cvs offcvs sock (DocState 0) (SyncState [])
  onpointerdown <- syncCallback1 ThrowWouldBlock (onPointerDown evar)
  js_addEventListener cvs "pointerdown" onpointerdown
  onpointermove <- syncCallback1 ThrowWouldBlock (onPointerMove evar)
  js_addEventListener cvs "pointermove" onpointermove
  onpointerup <- syncCallback1 ThrowWouldBlock (onPointerUp evar)
  js_addEventListener cvs "pointerup" onpointerup
  mdo
    rAF <- syncCallback ThrowWouldBlock (test cvs offcvs rAF)
    js_requestAnimationFrame rAF
  pure xstate

main :: IO ()
main = do
  putStrLn "new start"
  evar <- newEmptyMVar :: IO EventVar
  xstate <- setupCallback evar
  putMVar evar . Just $ D.driver simplelogger (world xstate initmc)
