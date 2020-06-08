{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Monad (forever, void, when)
import Data.Foldable (toList)
import qualified Data.JSString as JSS (pack,unpack)
import Data.Sequence (Seq, (|>), ViewR(..), singleton, viewr)
import GHCJS.Foreign.Callback (Callback, OnBlocked(ContinueAsync, ThrowWouldBlock)
                              , syncCallback, syncCallback1)
import GHCJS.Marshal (ToJSVal(toJSValListOf))
import GHCJS.Types (JSString, JSVal)
import qualified JavaScript.Web.MessageEvent as ME
import qualified JavaScript.Web.WebSocket as WS

import System.IO (hPutStrLn, hFlush, stdout)

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

data DrawingState = NotDrawing
                  | Drawing (Seq (Double,Double))

data DocState = DocState {
    _docstateCount :: Int
  }

data MyState = MyState {
    _mystateIsDrawing :: DrawingState
  , _mystateSVGBox :: JSVal
  , _mystateOverlayCanvas :: JSVal
  , _mystateOverlayOffCanvas :: JSVal
  , _mystateWebSocket :: WS.WebSocket
  , _mystateDocState :: DocState
  }

getXY :: JSVal -> IO (Double,Double)
getXY ev = (,) <$> js_clientX ev <*> js_clientY ev

onPointerDown :: TVar MyState -> JSVal -> IO ()
onPointerDown ref ev = do
  (x,y) <- getXY ev
  atomically $ modifyTVar' ref (\s -> s { _mystateIsDrawing = Drawing (singleton (x,y)) })


onPointerUp :: TVar MyState -> JSVal -> IO ()
onPointerUp ref ev = do
  MyState drawingState svg _ offcvs sock (DocState n) <- atomically $ readTVar ref
  case drawingState of
    Drawing xys -> do
      js_clear_overlay offcvs
      xy@(x,y) <- getXY ev

      let xys' = xys |> xy
      atomically $ modifyTVar' ref (\s -> s { _mystateIsDrawing = NotDrawing
                                            , _mystateDocState = DocState (n+1)
                                            })
      arr <- js_to_svg_point_array svg =<< toJSValListOf (toList xys')
      js_draw_path svg arr
      WS.send (JSS.pack (show (n+1))) sock
    _ -> pure ()

onPointerMove :: TVar MyState -> JSVal -> IO ()
onPointerMove ref ev = do
  MyState drawingState svg cvs offcvs _ _ <- atomically $ readTVar ref
  case drawingState of
    Drawing xys -> do
      xy@(x,y) <- getXY ev
      case viewr xys of
        _ :> (x0,y0) -> js_overlay_point cvs offcvs x0 y0 x y
        _ -> pure ()
      atomically $ modifyTVar' ref (\s -> s { _mystateIsDrawing = Drawing (xys |> xy)})
    _ ->
      pure ()

test :: JSVal -> JSVal -> Callback (IO ()) -> IO ()
test cvs offcvs rAF = do
  js_refresh cvs offcvs
  js_requestAnimationFrame rAF

main :: IO ()
main = do
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
  let wsClose _ = do
        hPutStrLn stdout "connection closed"
        hFlush stdout
      wsMessage ref msg = do
        let d = ME.getData msg
        case d of
          ME.StringData s -> do
            myst@(MyState _ _ _ _ _ (DocState n)) <- atomically $ readTVar ref
            let s' = read @Int (JSS.unpack s)
            hPutStrLn stdout (show s' ++ " <-> " ++ show n)
            when (s' > n) $
              atomically $ writeTVar ref (myst { _mystateDocState = DocState s' })
            hFlush stdout
          _ -> pure ()
  ref <- mdo
    ref <- newTVarIO (MyState NotDrawing svg cvs offcvs sock (DocState 0))
    sock <- WS.connect
              WS.WebSocketRequest {
                  WS.url = "ws://localhost:7080"
                , WS.protocols = []
                , WS.onClose = Just wsClose
                , WS.onMessage = Just (wsMessage ref)
                }
    pure ref

  onpointerdown <- syncCallback1 ThrowWouldBlock (onPointerDown ref)
  js_addEventListener cvs "pointerdown" onpointerdown

  onpointermove <- syncCallback1 ThrowWouldBlock (onPointerMove ref)
  js_addEventListener cvs "pointermove" onpointermove

  onpointerup   <- syncCallback1 ThrowWouldBlock (onPointerUp ref)
  js_addEventListener cvs "pointerup" onpointerup

  mdo
    rAF <- syncCallback ThrowWouldBlock (test cvs offcvs rAF)
    js_requestAnimationFrame rAF
