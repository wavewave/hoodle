{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Monad (forever, void, when)
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>), singleton)
import GHCJS.Foreign.Callback (Callback, OnBlocked(ContinueAsync, ThrowWouldBlock)
                              , syncCallback, syncCallback1)
import GHCJS.Marshal (ToJSVal(toJSValListOf))
import GHCJS.Types (JSString, JSVal)
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

foreign import javascript unsafe "refresh()"
  js_refresh :: IO ()

foreign import javascript unsafe "$1.addEventListener($2,$3)"
  js_addEventListener :: JSVal -> JSString -> Callback a -> IO ()

foreign import javascript unsafe "overlay_point($1,$2)"
  js_overlay_point :: Double -> Double -> IO ()

foreign import javascript unsafe "clear_overlay()"
  js_clear_overlay :: IO ()


data DrawingState = NotDrawing
                  | Drawing (Seq (Double,Double))

data MyState = MyState {
    _mystateIsDrawing :: DrawingState
  , _mystateSVGBox :: JSVal
  }

getXY :: JSVal -> IO (Double,Double)
getXY ev = (,) <$> js_clientX ev <*> js_clientY ev

onPointerDown :: TVar MyState -> JSVal -> IO ()
onPointerDown ref ev = do
  (x,y) <- getXY ev
  js_overlay_point x y
  atomically $ modifyTVar' ref (\s -> s { _mystateIsDrawing = Drawing (singleton (x,y)) })


onPointerUp :: TVar MyState -> JSVal -> IO ()
onPointerUp ref ev = do
  MyState drawingState svg <- atomically $ readTVar ref
  case drawingState of
    Drawing xys -> do
      js_clear_overlay
      xy@(x,y) <- getXY ev

      let xys' = xys |> xy
      atomically $ modifyTVar' ref (\s -> s { _mystateIsDrawing = NotDrawing })
      arr <- js_to_svg_point_array svg =<< toJSValListOf (toList xys')
      js_draw_path svg arr
    _ -> pure ()

onPointerMove :: TVar MyState -> JSVal -> IO ()
onPointerMove ref ev = do
  MyState drawingState svg <- atomically $ readTVar ref
  case drawingState of
    Drawing xys -> do
      xy@(x,y) <- getXY ev
      js_overlay_point x y
      atomically $ modifyTVar' ref (\s -> s { _mystateIsDrawing = Drawing (xys |> xy)})
    _ ->
      pure ()

test :: Callback (IO ()) -> IO ()
test rAF = do
  -- hPutStrLn stdout "test"
  -- hFlush stdout
  js_refresh
  js_requestAnimationFrame rAF

main :: IO ()
main = do
  putStrLn "ghcjs started"
  js_prevent_default_touch_move

  svg <- js_svg_box
  cvs <- js_canvas_overlay

  ref <- newTVarIO (MyState NotDrawing svg)

  onpointerdown <- syncCallback1 ThrowWouldBlock (onPointerDown ref)
  -- js_on svg "pointerdown" onpointerdown
  js_addEventListener cvs "pointerdown" onpointerdown

  onpointermove <- syncCallback1 ThrowWouldBlock (onPointerMove ref)
  -- js_on svg "pointermove" onpointermove
  js_addEventListener cvs "pointermove" onpointermove

  onpointerup   <- syncCallback1 ThrowWouldBlock (onPointerUp ref)
  -- js_on svg "pointerup" onpointerup
  js_addEventListener cvs "pointerup" onpointerup

  mdo
    rAF <- syncCallback ThrowWouldBlock (test rAF)
    js_requestAnimationFrame rAF
