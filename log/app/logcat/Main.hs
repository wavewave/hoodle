{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as E
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.GI.Base (AttrOp ((:=)), new, on)
import Data.GI.Base.ManagedPtr (withManagedPtr)
import Data.GI.Gtk.Threading (postGUIASync)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (group, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Generics
import GHC.RTS.Events (Event (..), EventInfo (..))
import GHC.RTS.Events.Incremental
  ( Decoder (..),
    decodeEvents,
    readEvents,
    readHeader,
  )
import qualified GI.Cairo.Render as R
import GI.Cairo.Render.Connector (renderWithContext)
import qualified GI.Gtk as Gtk
import Network.Socket
  ( Family (AF_UNIX),
    SockAddr (SockAddrUnix),
    Socket,
    SocketType (Stream),
    close,
    connect,
    socket,
    withSocketsDo,
  )
import Network.Socket.ByteString (recv)
import System.IO (hFlush, stdout)
import Text.Pretty.Simple (pPrint)
import Util (eventInfoToString, histo)

data MyState = MyState Double

recordEvent :: IORef Int -> Event -> IO ()
recordEvent ref ev = do
  modifyIORef' ref (+ 1)
  pPrint ev

dump :: IORef Int -> Socket -> IO ()
dump ref sock = goHeader ""
  where
    goHeader bs0 = do
      bs1 <- recv sock 1024
      let bs = bs0 <> bs1
      let lbs = BL.fromStrict bs
      let e = readHeader lbs
      case e of
        Left err -> print err >> goHeader bs
        Right (hdr, lbs') -> do
          pPrint hdr
          let dec0 = decodeEvents hdr
          goEvents hdr dec0 (BL.toStrict lbs')

    go :: Decoder Event -> BS.ByteString -> IO (Maybe (Decoder Event), BS.ByteString)
    go dec !bytes = do
      case dec of
        Produce ev dec' -> do
          recordEvent ref ev
          hFlush stdout
          go dec' bytes
        Consume k ->
          if BS.null bytes
            then pure (Just dec, "")
            else go (k bytes) ""
        Done bytes' ->
          pure (Nothing, bytes')
        Error bytes' e -> do
          pPrint e
          hFlush stdout
          -- reset if error happens.
          pure (Nothing, "")

    goEvents hdr dec !bytes = do
      (mdec', bytes') <- go dec bytes
      let dec' = fromMaybe (decodeEvents hdr) mdec'
      bytes'' <- recv sock 1024
      goEvents hdr dec' (bytes' <> bytes'')

myDraw :: IORef Int -> IORef MyState -> R.Render ()
myDraw ref ref' = do
  MyState x <- liftIO $ readIORef ref'
  R.setSourceRGBA 0.16 0.18 0.19 1.0
  R.setLineWidth (1.5 / 60)
  R.rectangle x x 50 50
  R.fill
  liftIO $ putStrLn "draw called"
  liftIO $ do
    n <- readIORef ref
    print n

tickTock :: Gtk.DrawingArea -> IORef MyState -> IO ()
tickTock drawingArea ref' = forever $ do
  threadDelay 50_000
  modifyIORef' ref' (\(MyState x) -> MyState (x + 1))
  postGUIASync $
    #queueDraw drawingArea

main :: IO ()
main = do
  ref <- newIORef 0
  ref' <- newIORef (MyState 10)

  _ <- Gtk.init Nothing
  mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
  drawingArea <- new Gtk.DrawingArea []
  drawingArea `on` #draw $
    renderWithContext $ do
      myDraw ref ref'
      pure True
  layout <- do
    vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]
    #packStart vbox drawingArea True True 0
    pure vbox
  #add mainWindow layout
  #showAll mainWindow

  _ <- forkIO $ tickTock drawingArea ref'
  _ <- forkIO (receiver ref)
  Gtk.main

receiver :: IORef Int -> IO ()
receiver ref =
  withSocketsDo $ do
    let file = "/tmp/eventlog.sock"
        open = do
          sock <- socket AF_UNIX Stream 0
          connect sock (SockAddrUnix file)
          pure sock
    E.bracket open close (dump ref)
    pure ()
