{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
-- import Control.Concurrent.MVar (MVar,takeMVar,newEmptyMVar)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad (forever, void)
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

data MyState = MyState { _mystateIsDrawing :: Bool }

onPointerDown :: JSVal -> IO ()
onPointerDown j = do
  hPutStrLn stdout "GHCJS: on pointerdown event"
  hFlush stdout
  js_eval j

onPointerUp :: JSVal -> IO ()
onPointerUp j = do
  hPutStrLn stdout "GHCJS: on pointerup event"
  hFlush stdout
  js_eval j


onPointerMove :: JSVal -> IO ()
onPointerMove j = do
  hPutStrLn stdout "GHCJS: on pointermove event"
  hFlush stdout
  js_eval j


main :: IO ()
main = do
  putStrLn "ghcjs started"
  js_prevent_default_touch_move

  onpointerdown <- syncCallback1 ThrowWouldBlock onPointerDown
  js_set_callback "onpointerdown" onpointerdown

  onpointermove <- syncCallback1 ThrowWouldBlock onPointerMove
  js_set_callback "onpointermove" onpointermove

  onpointerup   <- syncCallback1 ThrowWouldBlock onPointerUp
  js_set_callback "onpointerup"   onpointerup
