-- |
-- Module      : Hoodle.Widget.Clock
-- Copyright   : (c) 2013-2015 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Clock widget drawing and action
module Hoodle.Widget.Clock where

import Control.Lens (over, set, view, (.~))
import Control.Monad.State (get, liftIO, modify)
import Data.Functor.Identity (Identity (..))
import Data.Hoodle.BBox (BBox (..))
import Data.Hoodle.Simple (Dimension (Dim))
import Data.List (delete)
import Data.Time
  ( UTCTime,
    getCurrentTime,
  )
import Graphics.Hoodle.Render.Util.HitTest (isPointInBBox)
import qualified Graphics.Rendering.Cairo as Cairo
import Hoodle.Accessor (renderCache)
import Hoodle.Coroutine.Draw
  ( invalidate,
    invalidateInBBox,
    nextevent,
  )
import Hoodle.Coroutine.Pen (processWithDefTimeInterval)
import Hoodle.Device (PointerCoord)
import Hoodle.Type.Canvas
  ( CanvasId,
    CanvasInfo,
    canvasWidgets,
    forBoth,
    forBoth',
    unboxBiAct,
    unboxBiXform,
    unboxLens,
  )
import Hoodle.Type.Coroutine (MainCoroutine)
import Hoodle.Type.Enum (DrawFlag (Efficient))
import Hoodle.Type.Event (UserEvent (PenMove, PenUp))
import Hoodle.Type.HoodleState
  ( currentUnit,
    getCanvasInfo,
    getCurrentCanvasId,
    getHoodle,
    setCanvasInfo,
    unitHoodles,
  )
import Hoodle.Type.PageArrangement
  ( CanvasCoordinate (..),
    CanvasDimension (..),
  )
import Hoodle.Type.Widget
  ( WidgetItem (ClockWidget),
    allWidgets,
    clockWidgetConfig,
    clockWidgetPosition,
    doesUseClockWidget,
    widgetConfig,
  )
import Hoodle.View.Coordinate
  ( CanvasGeometry (canvasDim, desktop2Canvas),
    device2Desktop,
  )
import Hoodle.View.Draw
  ( canvasImageSurface,
    doubleBufferFlush,
    drawWidgets,
    renderClockWidget,
    virtualDoubleBufferDraw,
  )

-- |
newtype CWAction = Move (CanvasCoordinate, CanvasCoordinate)
  deriving (Show)

checkPointerInClock ::
  (CanvasId, CanvasInfo a, CanvasGeometry) ->
  PointerCoord ->
  Maybe CWAction
checkPointerInClock (_cid, cinfo, geometry) pcoord
  | b =
    let oxy@(CvsCoord (x, y)) = (desktop2Canvas geometry . device2Desktop geometry) pcoord
        owxy@(CvsCoord (x0, y0)) = view (canvasWidgets . clockWidgetConfig . clockWidgetPosition) cinfo
        obbox = BBox (x0 - 50, y0 - 50) (x0 + 50, y0 + 50)
        r
          | isPointInBBox obbox (x, y) = Just (Move (oxy, owxy))
          | otherwise = Nothing
     in r
  | otherwise = Nothing
  where
    b = view (canvasWidgets . widgetConfig . doesUseClockWidget) cinfo

-- |
startClockWidget ::
  (CanvasId, CanvasInfo a, CanvasGeometry) ->
  CWAction ->
  MainCoroutine ()
startClockWidget (_cid, cinfo, geometry) (Move (oxy, owxy)) = do
  xst <- get
  cache <- renderCache
  let uhdl = view (unitHoodles . currentUnit) xst
      hdl = getHoodle uhdl
      cid = getCurrentCanvasId uhdl
  (srcsfc, Dim wsfc hsfc) <- liftIO (canvasImageSurface cache cid Nothing geometry hdl)
  -- need to draw other widgets here
  let otherwidgets = delete ClockWidget allWidgets
  liftIO $ Cairo.renderWith srcsfc (drawWidgets otherwidgets hdl cinfo Nothing)
  -- end : need to draw other widgets here ^^^
  tgtsfc <-
    liftIO $
      Cairo.createImageSurface
        Cairo.FormatARGB32
        (floor wsfc)
        (floor hsfc)
  ctime <- liftIO getCurrentTime
  manipulateCW cid geometry (srcsfc, tgtsfc) owxy oxy ctime
  liftIO $ Cairo.surfaceFinish srcsfc
  liftIO $ Cairo.surfaceFinish tgtsfc

-- | main event loop for clock widget
manipulateCW ::
  CanvasId ->
  CanvasGeometry ->
  (Cairo.Surface, Cairo.Surface) ->
  CanvasCoordinate ->
  CanvasCoordinate ->
  UTCTime ->
  MainCoroutine ()
manipulateCW cid geometry (srcsfc, tgtsfc) owxy oxy otime = do
  r <- nextevent
  case r of
    PenMove _ pcoord -> do
      processWithDefTimeInterval
        (manipulateCW cid geometry (srcsfc, tgtsfc) owxy oxy)
        ( \ctime ->
            moveClockWidget cid geometry (srcsfc, tgtsfc) owxy oxy pcoord
              >> manipulateCW cid geometry (srcsfc, tgtsfc) owxy oxy ctime
        )
        otime
    PenUp _ _ -> invalidate cid
    _ -> return ()

moveClockWidget ::
  CanvasId ->
  CanvasGeometry ->
  (Cairo.Surface, Cairo.Surface) ->
  CanvasCoordinate ->
  CanvasCoordinate ->
  PointerCoord ->
  MainCoroutine ()
moveClockWidget cid geometry (srcsfc, tgtsfc) (CvsCoord (xw, yw)) (CvsCoord (x0, y0)) pcoord = do
  let CvsCoord (x, y) = (desktop2Canvas geometry . device2Desktop geometry) pcoord
  modify $ \xst ->
    let uhdl = view (unitHoodles . currentUnit) xst
        CanvasDimension (Dim cw ch) = canvasDim geometry
        cinfobox = getCanvasInfo cid uhdl
        nposx
          | xw + x - x0 < -50 = -50
          | xw + x - x0 > cw - 50 = cw - 50
          | otherwise = xw + x - x0
        nposy
          | yw + y - y0 < -50 = -50
          | yw + y - y0 > ch - 50 = ch - 50
          | otherwise = yw + y - y0
        nwpos = CvsCoord (nposx, nposy)
        changeact :: CanvasInfo a -> CanvasInfo a
        changeact = set (canvasWidgets . clockWidgetConfig . clockWidgetPosition) nwpos
        ncinfobox = (runIdentity . forBoth unboxBiXform (return . changeact)) cinfobox
     in (unitHoodles . currentUnit .~ setCanvasInfo (cid, ncinfobox) uhdl) xst
  --
  xst2 <- get
  let uhdl = view (unitHoodles . currentUnit) xst2
      cinfobox2 = getCanvasInfo cid uhdl
      cfg = view (unboxLens (canvasWidgets . clockWidgetConfig)) cinfobox2
  liftIO $
    forBoth'
      unboxBiAct
      ( \cinfo ->
          virtualDoubleBufferDraw
            srcsfc
            tgtsfc
            (return ())
            (renderClockWidget Nothing cfg)
            >> doubleBufferFlush tgtsfc cinfo
      )
      cinfobox2

-- |
toggleClock :: CanvasId -> MainCoroutine ()
toggleClock cid = do
  modify $ \xst ->
    let uhdl = view (unitHoodles . currentUnit) xst
        ncinfobox =
          ( over (unboxLens (canvasWidgets . widgetConfig . doesUseClockWidget)) not
              . getCanvasInfo cid
          )
            uhdl
     in (unitHoodles . currentUnit .~ setCanvasInfo (cid, ncinfobox) uhdl) xst
  invalidateInBBox Nothing Efficient cid
