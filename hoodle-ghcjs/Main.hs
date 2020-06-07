{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Monad (forever, void, when)
import Data.Foldable (toList)
import Data.Sequence (Seq, (|>), singleton)
import GHCJS.Foreign.Callback (Callback, OnBlocked(ContinueAsync, ThrowWouldBlock)
                              , syncCallback1)
import GHCJS.Marshal (ToJSVal(toJSValListOf))
import GHCJS.Types (JSString, JSVal)
import System.IO (hPutStrLn, hFlush, stdout)

foreign import javascript unsafe "console.log($1)"
  js_console_log :: JSVal -> IO ()

foreign import javascript unsafe "preventDefaultTouchMove()"
  js_prevent_default_touch_move :: IO ()

foreign import javascript unsafe "$r = SVG('#box')"
  js_svg_box :: IO JSVal

foreign import javascript unsafe "startLineBit($1,$2)"
  js_start_line_bit :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "drawLineBit($1,$2)"
  js_draw_line_bit :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "endLineBit($1,$2)"
  js_end_line_bit :: JSVal -> JSVal -> IO ()

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
  atomically $ modifyTVar' ref (\s -> s { _mystateIsDrawing = Drawing (singleton (x,y)) })
  -- MyState _ svg <- atomically $ readTVar ref
  -- js_start_line_bit svg ev

onPointerUp :: TVar MyState -> JSVal -> IO ()
onPointerUp ref ev = do
  MyState drawingState svg <- atomically $ readTVar ref
  case drawingState of
    Drawing xys -> do
      xy <- getXY ev
      let xys' = xys |> xy
      atomically $ modifyTVar' ref (\s -> s { _mystateIsDrawing = NotDrawing })
      -- j <- toJSValListOf (toList xys')
      -- js_console_log j
      arr <- js_to_svg_point_array svg =<< toJSValListOf (toList xys')
      -- js_end_line_bit svg ev
      js_draw_path svg arr
    _ -> pure ()

onPointerMove :: TVar MyState -> JSVal -> IO ()
onPointerMove ref ev = do
  MyState drawingState svg <- atomically $ readTVar ref
  case drawingState of
    Drawing xys -> do
      xy <- getXY ev
      atomically $ modifyTVar' ref (\s -> s { _mystateIsDrawing = Drawing (xys |> xy)})
      -- js_draw_line_bit svg ev
    _ ->
      pure ()

main :: IO ()
main = do
  putStrLn "ghcjs started"
  js_prevent_default_touch_move

  svg <- js_svg_box

  ref <- newTVarIO (MyState NotDrawing svg)

  onpointerdown <- syncCallback1 ThrowWouldBlock (onPointerDown ref)
  js_on svg "pointerdown" onpointerdown

  onpointermove <- syncCallback1 ThrowWouldBlock (onPointerMove ref)
  js_on svg "pointermove" onpointermove


  onpointerup   <- syncCallback1 ThrowWouldBlock (onPointerUp ref)
  js_on svg "pointerup" onpointerup
