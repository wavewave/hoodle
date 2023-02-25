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
import Data.Fixed (Fixed (MkFixed), Nano)
import Data.Foldable (for_, toList)
import Data.GI.Base (AttrOp ((:=)), new, on)
import Data.GI.Gtk.Threading (postGUIASync)
import qualified Data.List as L (foldl', lookup)
import Data.Map (Map)
import qualified Data.Map as Map (empty, toAscList)
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
import Util.Event (eventInfoEnumMap, eventInfoToString)
import Util.Histo (aggregateCount, histoAdd)

data LogcatState = LogcatState
  { _logcatEventStore :: Seq Event,
    -- TODO: Queue should be a local state, not a global state, considering STM overhead.
    _logcatEventQueue :: Seq Event,
    _logcatEventHisto :: Map String Int
  }

makeLenses ''LogcatState

emptyLogcatState :: LogcatState
emptyLogcatState = LogcatState Seq.empty Seq.empty Map.empty

recordEvent :: TVar LogcatState -> Event -> IO ()
recordEvent sref ev =
  atomically $ modifyTVar' sref (logcatEventQueue %~ (|> ev))

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

canvasWidth :: Double
canvasWidth = 1440

canvasHeight :: Double
canvasHeight = 768

secToPixel :: Nano -> Double
secToPixel sec =
  realToFrac sec * 10.0 + 10

pixelToSec :: Double -> Nano
pixelToSec px =
  realToFrac ((px - 10.0) / 10.0)

drawEventMark :: Event -> R.Render ()
drawEventMark ev = do
  let sec = MkFixed (fromIntegral (evTime ev)) :: Nano
      --  x :: Double
      x = secToPixel sec -- realToFrac sec * 10.0 + 10
      evname = eventInfoToString (evSpec ev)
      tag = fromMaybe 0 (L.lookup evname eventInfoEnumMap)
      y = fromIntegral tag * 3.0
  R.moveTo x y
  R.lineTo x (y + 2)
  R.stroke

drawTimeGrid :: R.Render ()
drawTimeGrid = do
  let tmax = pixelToSec canvasWidth
      ts = [0, 1 .. tmax]
      lblTs = [0, 10 .. tmax]
  R.setSourceRGBA 0 0 1 0.5
  R.setLineWidth 0.1
  R.setLineCap R.LineCapRound
  R.setLineJoin R.LineJoinRound
  for_ ts $ \t -> do
    let x = secToPixel t
    R.moveTo x 0
    R.lineTo x 150
    R.stroke
  R.setSourceRGBA 0 0 1 0.8
  R.setFontSize 8
  for_ lblTs $ \t -> do
    R.moveTo (secToPixel t) 10
    R.textPath (show (floor t :: Int) <> " s")
    R.stroke

drawTimeline :: Seq Event -> R.Render ()
drawTimeline evs = do
  drawTimeGrid
  R.setSourceRGBA 0.16 0.18 0.19 1.0
  R.setLineWidth 0.3
  R.setLineCap R.LineCapRound
  R.setLineJoin R.LineJoinRound
  for_ evs $ \ev ->
    drawEventMark ev

drawHistBar :: (Double, Double) -> (String, Int) -> R.Render ()
drawHistBar (xoffset, yoffset) (ev, value) = do
  let tag = fromMaybe 0 (L.lookup ev eventInfoEnumMap)
  R.setSourceRGBA 0.16 0.18 0.19 1.0
  R.setLineWidth 1.0
  let y = yoffset + 10.0 * fromIntegral tag
      w = fromIntegral value / 100.0
  R.moveTo xoffset (y + 10.0)
  R.setFontSize 8.0
  R.textPath ev
  R.fill
  R.rectangle (xoffset + 100) (y + 2) w 8
  R.fill
  R.moveTo (xoffset + 104 + w) (y + 10.0)
  R.textPath (show value)
  R.fill

drawLogcatState :: TVar LogcatState -> R.Render ()
drawLogcatState sref = do
  R.setSourceRGB 1 1 1
  R.rectangle 0 0 canvasWidth canvasHeight
  R.fill
  s <- liftIO $ atomically $ readTVar sref
  let evs = s ^. logcatEventStore
      hist = s ^. logcatEventHisto
  let xoffset = 10
      yoffset = 100
  drawTimeline evs
  R.setSourceRGBA 0 0 1 0.8
  R.setLineWidth 1
  R.moveTo 0 150
  R.lineTo canvasWidth 150
  R.stroke
  for_ (Map.toAscList hist) $ \(ev, value) ->
    drawHistBar (xoffset, yoffset) (ev, value)

flushDoubleBuffer :: R.Surface -> R.Render ()
flushDoubleBuffer sfc = do
  R.setSourceSurface sfc 0 0
  R.setOperator R.OperatorSource
  R.paint

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
