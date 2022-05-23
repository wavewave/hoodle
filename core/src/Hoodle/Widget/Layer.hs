module Hoodle.Widget.Layer where

import Control.Lens (over, set, view, (.~))
import Control.Monad.State
import Data.Functor.Identity (Identity (..))
import Data.Hoodle.BBox
import Data.Hoodle.Simple
import Data.List (delete)
import Data.Sequence
import Data.Time
import Graphics.Hoodle.Render.Util.HitTest
import qualified Graphics.Rendering.Cairo as Cairo
import Hoodle.Accessor
import Hoodle.Coroutine.Draw
import Hoodle.Coroutine.Layer
import Hoodle.Coroutine.Pen
import Hoodle.Device
import Hoodle.HitTest (hitLassoPoint)
import Hoodle.Type.Canvas
import Hoodle.Type.Coroutine
import Hoodle.Type.Enum
import Hoodle.Type.Event
import Hoodle.Type.HoodleState
import Hoodle.Type.PageArrangement
import Hoodle.Type.Widget
import Hoodle.View.Coordinate
import Hoodle.View.Draw

-- |
data LWAction = Close | ToggleShowContent | Move (CanvasCoordinate, CanvasCoordinate)

checkPointerInLayer ::
  (CanvasId, CanvasInfo a, CanvasGeometry) ->
  PointerCoord ->
  Maybe LWAction
checkPointerInLayer (_cid, cinfo, geometry) pcoord
  | b =
    let oxy@(CvsCoord (x, y)) = (desktop2Canvas geometry . device2Desktop geometry) pcoord
        owxy@(CvsCoord (x0, y0)) = view (canvasWidgets . layerWidgetConfig . layerWidgetPosition) cinfo
        obbox = BBox (x0, y0) (x0 + 100, y0 + 100)
        closebbox = BBox (x0, y0) (x0 + 10, y0 + 10)
        r
          | isPointInBBox closebbox (x, y) = Just Close
          | hitLassoPoint (fromList [(x0 + 90, y0 + 40), (x0 + 100, y0 + 50), (x0 + 90, y0 + 60)]) (x, y) = Just ToggleShowContent
          | isPointInBBox obbox (x, y) = Just (Move (oxy, owxy))
          | otherwise = Nothing
     in r
  | otherwise = Nothing
  where
    b = view (canvasWidgets . widgetConfig . doesUseLayerWidget) cinfo

startLayerWidget ::
  (CanvasId, CanvasInfo a, CanvasGeometry) ->
  LWAction ->
  MainCoroutine ()
startLayerWidget (cid, _cinfo, _geometry) Close = toggleLayer cid
startLayerWidget (cid, _cinfo, _geometry) ToggleShowContent = do
  modify $
    over (unitHoodles . currentUnit) $
      over (currentCanvasInfo . unboxLens (canvasWidgets . layerWidgetConfig . layerWidgetShowContent)) not
  invalidate cid
startLayerWidget (cid, cinfo, geometry) (Move (oxy, owxy)) = do
  xst <- get
  cache <- renderCache
  let uhdl = view (unitHoodles . currentUnit) xst
      hdl = getHoodle uhdl
  (srcsfc, Dim wsfc hsfc) <- liftIO (canvasImageSurface cache cid Nothing geometry hdl)
  -- need to draw other widgets here
  let otherwidgets = delete LayerWidget allWidgets
  liftIO $ Cairo.renderWith srcsfc (drawWidgets otherwidgets hdl cinfo Nothing)
  -- end : need to draw other widgets here ^^^
  tgtsfc <-
    liftIO $
      Cairo.createImageSurface
        Cairo.FormatARGB32
        (floor wsfc)
        (floor hsfc)
  ctime <- liftIO getCurrentTime
  let CvsCoord (x0, y0) = owxy
      CvsCoord (x, y) = oxy
      act
        | hitLassoPoint (fromList [(x0 + 80, y0), (x0 + 100, y0), (x0 + 100, y0 + 20)]) (x, y) = gotoNextLayer
        | hitLassoPoint (fromList [(x0, y0 + 80), (x0, y0 + 100), (x0 + 20, y0 + 100)]) (x, y) = gotoPrevLayer
        | otherwise = manipulateLW cid geometry (srcsfc, tgtsfc) owxy oxy ctime
  act
  liftIO $ Cairo.surfaceFinish srcsfc
  liftIO $ Cairo.surfaceFinish tgtsfc

-- | main event loop for layer widget
manipulateLW ::
  CanvasId ->
  CanvasGeometry ->
  (Cairo.Surface, Cairo.Surface) ->
  CanvasCoordinate ->
  CanvasCoordinate ->
  UTCTime ->
  MainCoroutine ()
manipulateLW cid geometry (srcsfc, tgtsfc) owxy oxy otime = do
  r <- nextevent
  case r of
    PenMove _ pcoord -> do
      processWithDefTimeInterval
        (manipulateLW cid geometry (srcsfc, tgtsfc) owxy oxy)
        ( \ctime ->
            moveLayerWidget cid geometry (srcsfc, tgtsfc) owxy oxy pcoord
              >> manipulateLW cid geometry (srcsfc, tgtsfc) owxy oxy ctime
        )
        otime
    PenUp _ _pcoord -> invalidate cid
    _ -> return ()

moveLayerWidget ::
  CanvasId ->
  CanvasGeometry ->
  (Cairo.Surface, Cairo.Surface) ->
  CanvasCoordinate ->
  CanvasCoordinate ->
  PointerCoord ->
  MainCoroutine ()
moveLayerWidget cid geometry (srcsfc, tgtsfc) (CvsCoord (xw, yw)) (CvsCoord (x0, y0)) pcoord = do
  let CvsCoord (x, y) = (desktop2Canvas geometry . device2Desktop geometry) pcoord
  get >>= \xst -> do
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
        changeact = set (canvasWidgets . layerWidgetConfig . layerWidgetPosition) nwpos
        ncinfobox = (runIdentity . forBoth unboxBiXform (return . changeact)) cinfobox
    put $ (unitHoodles . currentUnit .~ setCanvasInfo (cid, ncinfobox) uhdl) xst
    --
    xst2 <- get
    let uhdl2 = view (unitHoodles . currentUnit) xst2
        hdl2 = getHoodle uhdl2
        cinfobox2 = getCanvasInfo cid uhdl2
    liftIO $
      forBoth'
        unboxBiAct
        ( \cinfo ->
            virtualDoubleBufferDraw
              srcsfc
              tgtsfc
              (return ())
              (drawLayerWidget hdl2 cinfo Nothing nwpos)
              >> doubleBufferFlush tgtsfc cinfo
        )
        cinfobox2

-- |
toggleLayer :: CanvasId -> MainCoroutine ()
toggleLayer cid = do
  pureUpdateUhdl $ \uhdl ->
    let ncinfobox =
          ( over (unboxLens (canvasWidgets . widgetConfig . doesUseLayerWidget)) not
              . getCanvasInfo cid
          )
            uhdl
     in setCanvasInfo (cid, ncinfobox) uhdl
  invalidateInBBox Nothing Efficient cid
