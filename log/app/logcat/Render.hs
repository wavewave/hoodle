module Render
  ( canvasWidth,
    canvasHeight,
    secToPixel,
    pixelToSec,
    drawEventMark,
    drawTimeGrid,
    drawTimeline,
    drawHistBar,
    drawLogcatState,
    flushDoubleBuffer,
  )
where

import Control.Concurrent.STM (TVar, atomically, readTVar)
import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Fixed (Fixed (MkFixed), Nano)
import Data.Foldable (for_)
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import GHC.RTS.Events (Event (..))
import qualified GI.Cairo.Render as R
import Types (HasLogcatState (..), LogcatState)
import Util.Event (eventInfoEnumMap, eventInfoToString)

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
