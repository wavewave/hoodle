{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
-- import Control.Concurrent.MVar (MVar,takeMVar,newEmptyMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Monad (forever, void, when)
import GHCJS.Foreign.Callback (Callback, OnBlocked(ContinueAsync, ThrowWouldBlock)
                              , syncCallback1)
import GHCJS.Types (JSString, JSVal)
import System.IO (hPutStrLn, hFlush, stdout)

foreign import javascript unsafe "preventDefaultTouchMove()"
  js_prevent_default_touch_move :: IO ()

foreign import javascript unsafe "callback[$1] = $2"
  js_set_callback :: JSString -> Callback a -> IO ()

foreign import javascript unsafe "$1()"
  js_eval :: JSVal -> IO ()

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

data MyState = MyState {
    _mystateIsDrawing :: Bool
  , _mystateSVGBox :: JSVal
  }

onPointerDown :: TVar MyState -> JSVal -> IO ()
onPointerDown ref ev = do
  hPutStrLn stdout "GHCJS: on pointerdown event"
  hFlush stdout
  atomically $ modifyTVar' ref (\s -> s { _mystateIsDrawing = True })
  MyState _ svg <- atomically $ readTVar ref
  js_start_line_bit svg ev

  -- js_eval j

onPointerUp :: TVar MyState -> JSVal -> IO ()
onPointerUp ref ev = do
  hPutStrLn stdout "GHCJS: on pointerup event"
  hFlush stdout
  atomically $ modifyTVar' ref (\s -> s { _mystateIsDrawing = False })
  MyState _ svg <- atomically $ readTVar ref
  js_end_line_bit svg ev

  -- js_eval j


onPointerMove :: TVar MyState -> JSVal -> IO ()
onPointerMove ref ev = do
  hPutStrLn stdout "GHCJS: on pointermove event"
  hFlush stdout
  MyState isDrawing svg <- atomically $ readTVar ref
  when isDrawing $
    js_draw_line_bit svg ev


main :: IO ()
main = do
  putStrLn "ghcjs started"
  js_prevent_default_touch_move

  svg <- js_svg_box

  ref <- newTVarIO (MyState False svg)

  onpointerdown <- syncCallback1 ThrowWouldBlock (onPointerDown ref)
  -- js_set_callback "onpointerdown" onpointerdown
  js_on svg "pointerdown" onpointerdown

  onpointermove <- syncCallback1 ThrowWouldBlock (onPointerMove ref)
  -- js_set_callback "onpointermove" onpointermove
  js_on svg "pointermove" onpointermove


  onpointerup   <- syncCallback1 ThrowWouldBlock (onPointerUp ref)
  -- js_set_callback "onpointerup"   onpointerup
  js_on svg "pointerup" onpointerup
