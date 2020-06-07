{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
-- import Control.Concurrent.MVar (MVar,takeMVar,newEmptyMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
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

data MyState = MyState { _mystateIsDrawing :: Bool }

onPointerDown :: TVar MyState -> JSVal -> IO ()
onPointerDown ref j = do
  hPutStrLn stdout "GHCJS: on pointerdown event"
  hFlush stdout
  atomically $ writeTVar ref (MyState True)
  js_eval j

onPointerUp :: TVar MyState -> JSVal -> IO ()
onPointerUp ref j = do
  hPutStrLn stdout "GHCJS: on pointerup event"
  hFlush stdout
  atomically $ writeTVar ref (MyState False)
  js_eval j


onPointerMove :: TVar MyState -> JSVal -> IO ()
onPointerMove ref j = do
  hPutStrLn stdout "GHCJS: on pointermove event"
  hFlush stdout
  MyState isDrawing <- atomically $ readTVar ref
  when isDrawing $
    js_eval j


main :: IO ()
main = do
  putStrLn "ghcjs started"
  js_prevent_default_touch_move

  ref <- newTVarIO (MyState False)

  onpointerdown <- syncCallback1 ThrowWouldBlock (onPointerDown ref)
  js_set_callback "onpointerdown" onpointerdown

  onpointermove <- syncCallback1 ThrowWouldBlock (onPointerMove ref)
  js_set_callback "onpointermove" onpointermove

  onpointerup   <- syncCallback1 ThrowWouldBlock (onPointerUp ref)
  js_set_callback "onpointerup"   onpointerup
