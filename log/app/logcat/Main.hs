{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar)
import qualified Control.Exception as E
import Control.Lens (makeLenses, (%~), (.~), (^.))
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.GI.Base (AttrOp ((:=)), new, on)
import Data.GI.Gtk.Threading (postGUIASync)
-- import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq (empty)
import GHC.RTS.Events (Event (..))
import GHC.RTS.Events.Incremental
  ( Decoder (..),
    decodeEvents,
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

data LogcatState = LogcatState
  { _logcatEventStore :: Seq Event,
    -- TODO: Queue should be a local state, not a global state, considering STM overhead.
    _logcatEventQueue :: Seq Event,
    _logcatEventHisto :: [(String, Int)]
  }

makeLenses ''LogcatState

emptyLogcatState :: LogcatState
emptyLogcatState = LogcatState Seq.empty Seq.empty []

recordEvent :: TVar LogcatState -> Event -> IO ()
recordEvent sref ev =
  atomically $ modifyTVar' sref (logcatEventQueue %~ (|> ev))

flushEventQueue :: TVar LogcatState -> IO ()
flushEventQueue sref = do
  h <- atomically $ do
    queue <- (^. logcatEventQueue) <$> readTVar sref
    let h = histo $ fmap (eventInfoToString . evSpec) $ toList queue
    modifyTVar' sref $
      (logcatEventStore %~ (<> queue))
        . (logcatEventQueue .~ Seq.empty)
    pure h
  print h

dump :: TVar LogcatState -> Socket -> IO ()
dump sref sock = goHeader ""
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
          recordEvent sref ev
          hFlush stdout
          go dec' bytes
        Consume k ->
          if BS.null bytes
            then pure (Just dec, "")
            else go (k bytes) ""
        Done bytes' ->
          pure (Nothing, bytes')
        Error _bytes' e -> do
          pPrint e
          hFlush stdout
          -- reset if error happens.
          pure (Nothing, "")

    goEvents hdr dec !bytes = do
      (mdec', bytes') <- go dec bytes
      let dec' = fromMaybe (decodeEvents hdr) mdec'
      bytes'' <- recv sock 1024
      goEvents hdr dec' (bytes' <> bytes'')

myDraw :: TVar LogcatState -> R.Render ()
myDraw sref = do
  evs <-
    liftIO $
      (^. logcatEventStore) <$> atomically (readTVar sref)
  let x = fromIntegral (length evs) / 1000.0
  R.setSourceRGBA 0.16 0.18 0.19 1.0
  R.setLineWidth (1.5 / 60)
  R.rectangle x 10 50 50
  R.fill

tickTock :: Gtk.DrawingArea -> TVar LogcatState -> IO ()
tickTock drawingArea sref = forever $ do
  threadDelay 1_000_000
  flushEventQueue sref
  postGUIASync $
    #queueDraw drawingArea

main :: IO ()
main = do
  sref <- newTVarIO emptyLogcatState

  _ <- Gtk.init Nothing
  mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
  drawingArea <- new Gtk.DrawingArea []
  _ <- drawingArea `on` #draw $
    renderWithContext $ do
      myDraw sref
      pure True
  layout <- do
    vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]
    #packStart vbox drawingArea True True 0
    pure vbox
  #add mainWindow layout
  #showAll mainWindow

  _ <- forkIO $ tickTock drawingArea sref
  _ <- forkIO $ receiver sref
  Gtk.main

receiver :: TVar LogcatState -> IO ()
receiver sref =
  withSocketsDo $ do
    let file = "/tmp/eventlog.sock"
        open = do
          sock <- socket AF_UNIX Stream 0
          connect sock (SockAddrUnix file)
          pure sock
    E.bracket open close (dump sref)
    pure ()
