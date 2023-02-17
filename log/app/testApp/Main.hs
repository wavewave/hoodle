{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -w #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Debug.Trace (flushEventLog, traceEventIO)
import Foreign.Ptr (FunPtr)
import GHC.Eventlog.Socket (startWait)
import qualified Graphics.UI.Gtk as Gtk

foreign import ccall safe "cbit.h callTest" c_callTest :: IO ()

type SimpleIO = IO ()

foreign import ccall "wrapper" mkWrapper :: SimpleIO -> IO (FunPtr SimpleIO)

foreign import ccall safe "cbit.h callTest2" c_callTest2 :: FunPtr SimpleIO -> IO ()

simpleAction :: IO ()
simpleAction =
  putStrLn "I am in simple action!"

main :: IO ()
main = do
  startWait "/tmp/eventlog.sock"
  wSimpleAction <- mkWrapper simpleAction
  forkIO $
    forever $ do
      threadDelay 1_000_000
      flushEventLog
  --forkIO $
  forever $ do
    threadDelay 5_000_000
    putStrLn "am i here"
    for_ [1..100000] $ \_ ->
      c_callTest
    -- c_callTest2 wSimpleAction

  {- _ <- Gtk.initGUI
  window <- Gtk.windowNew
  box <- Gtk.vBoxNew False 0
  Gtk.containerAdd window box
  cvs <- Gtk.drawingAreaNew
  Gtk.boxPackStart box cvs Gtk.PackGrow 0
  {- _ <- cvs `Gtk.on` Gtk.motionNotifyEvent $
    Gtk.tryEvent $ do
      liftIO $ traceEventIO "user event"
  Gtk.widgetAddEvents cvs [Gtk.PointerMotionMask, Gtk.Button1MotionMask, Gtk.KeyPressMask]

  _ <- cvs `Gtk.on` Gtk.buttonPressEvent $
    Gtk.tryEvent $ do
      liftIO $ traceEventIO "user event"
  -}
  Gtk.widgetShowAll window
  Gtk.mainGUI
  -}