{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar)
import qualified Control.Exception as E
import Control.Lens ((%~), (.~), (^.))
import Control.Monad (forever)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Fixed (Fixed (MkFixed))
import Data.Foldable (toList)
import Data.GI.Base (AttrOp ((:=)), new, on)
import Data.GI.Gtk.Threading (postGUIASync)
import qualified Data.List as L (foldl')
import Data.Maybe (fromMaybe)
import Data.Sequence ((|>))
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
import Render
  ( canvasHeight,
    canvasWidth,
    drawLogcatState,
    flushDoubleBuffer,
    pixelToSec,
    secToPixel,
    timelineMargin,
  )
import System.IO (hFlush, stdout)
import Text.Pretty.Simple (pPrint)
import Types
  ( HasLogcatState (..),
    HasViewState (..),
    LogcatState,
    emptyLogcatState,
  )
import Util.Event (eventInfoToString)
import Util.Histo (aggregateCount, histoAdd)

recordEvent :: TVar LogcatState -> Event -> IO ()
recordEvent sref ev =
  atomically $ do
    ltime <- (^. logcatLastEventTime) <$> readTVar sref
    let sec = MkFixed (fromIntegral (evTime ev))
        updateLastEventTime =
          if sec > ltime
            then logcatLastEventTime .~ sec
            else id
    modifyTVar' sref ((logcatEventQueue %~ (|> ev)) . updateLastEventTime)

-- | Adjust timeline viewport to ensure the last event is out of the right margin
-- of the timeline. This checks if the last event falls under the margin, and if so,
-- move the plot origin to make the last event at the center of the timeline.
adjustTimelineOrigin :: LogcatState -> LogcatState
adjustTimelineOrigin s
  | ltimePos > canvasWidth - timelineMargin =
    let currCenterTime = pixelToSec origin (canvasWidth * 0.5)
        deltaTime = ltime - currCenterTime
     in (logcatViewState . viewTimeOrigin %~ (\x -> x + deltaTime)) s
  | otherwise = s
  where
    origin = s ^. logcatViewState . viewTimeOrigin
    ltime = s ^. logcatLastEventTime
    ltimePos = secToPixel origin ltime

flushEventQueue :: R.Surface -> TVar LogcatState -> IO ()
flushEventQueue sfc sref = do
  atomically $ do
    s <- readTVar sref
    let queue = s ^. logcatEventQueue
        hist = s ^. logcatEventHisto
        diff = aggregateCount $ fmap (eventInfoToString . evSpec) $ toList queue
        hist' = L.foldl' histoAdd hist diff
    modifyTVar' sref $
      (logcatEventStore %~ (<> queue))
        . (logcatEventQueue .~ Seq.empty)
        . (logcatEventHisto .~ hist')
        . adjustTimelineOrigin
  R.renderWith sfc $ do
    drawLogcatState sref

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

tickTock :: Gtk.DrawingArea -> R.Surface -> TVar LogcatState -> IO ()
tickTock drawingArea sfc sref = forever $ do
  threadDelay 1_000_000
  flushEventQueue sfc sref
  postGUIASync $
    #queueDraw drawingArea

main :: IO ()
main = do
  sref <- newTVarIO emptyLogcatState
  _ <- Gtk.init Nothing
  -- NOTE: this should be closed with surfaceDestroy
  sfc <- R.createImageSurface R.FormatARGB32 (floor canvasWidth) (floor canvasHeight)
  mainWindow <- new Gtk.Window [#type := Gtk.WindowTypeToplevel]
  drawingArea <- new Gtk.DrawingArea []
  _ <- drawingArea `on` #draw $
    renderWithContext $ do
      flushDoubleBuffer sfc
      pure True
  layout <- do
    vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]
    #packStart vbox drawingArea True True 0
    pure vbox
  #add mainWindow layout
  #setDefaultSize mainWindow (floor canvasWidth) (floor canvasHeight)
  #showAll mainWindow

  _ <- forkIO $ tickTock drawingArea sfc sref
  _ <- forkIO $ receiver sref
  Gtk.main
  R.surfaceFinish sfc

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
