{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

-- |
-- Module      : Hoodle.Widget.PanZoom
-- Copyright   : (c) 2013-2015 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Pan-Zoom widget drawing and action
module Hoodle.Widget.PanZoom where

import Control.Lens (over, set, view, (.~))
import Control.Monad (void, when)
import Control.Monad.Identity (Identity (..))
import Control.Monad.State (get, gets, liftIO, modify)
import Data.Hoodle.BBox (BBox (..))
import Data.Hoodle.Simple (Dimension (..))
import Data.List (delete)
import Data.Time.Clock
  ( UTCTime,
    getCurrentTime,
  )
import Graphics.Hoodle.Render.Util.HitTest (isPointInBBox)
import qualified Graphics.Rendering.Cairo as Cairo
import Hoodle.Accessor
  ( getCanvasGeometryCvsId,
    lensSetToggleUIForFlag,
    pureUpdateUhdl,
    renderCache,
  )
import Hoodle.Coroutine.Draw
  ( invalidate,
    invalidateAll,
    invalidateInBBox,
    nextevent,
    updateFlagFromToggleUI,
    waitSomeEvent,
  )
import Hoodle.Coroutine.Page (canvasZoomUpdateGenRenderCvsId)
import Hoodle.Coroutine.Pen (processWithDefTimeInterval)
import Hoodle.Coroutine.Scroll (moveViewPortBy)
import Hoodle.Device
  ( PointerCoord,
    dev_touch_str,
  )
import Hoodle.ModelAction.Page (zoomRatioFrmRelToCurr)
import Hoodle.Type.Canvas
  ( CanvasId,
    CanvasInfo,
    canvasWidgets,
    currentPageNum,
    drawArea,
    forBoth,
    forBoth',
    pageArrangement,
    unboxBiAct,
    unboxBiXform,
    unboxLens,
    viewInfo,
  )
import Hoodle.Type.Coroutine (MainCoroutine, doIOaction)
import Hoodle.Type.Enum (DrawFlag (Efficient))
import Hoodle.Type.Event
  ( AllEvent (UsrEv),
    UserEvent
      ( ActionOrdered,
        PenMove,
        PenUp,
        TouchMove,
        TouchUp
      ),
  )
import Hoodle.Type.HoodleState
  ( currentCanvas,
    currentCanvasInfo,
    currentUnit,
    deviceList,
    doesNotInvalidate,
    doesUseTouch,
    getCanvasInfo,
    getHoodle,
    setCanvasInfo,
    settings,
    unitHoodles,
  )
import Hoodle.Type.PageArrangement
  ( CanvasCoordinate (..),
    CanvasDimension (..),
    DesktopCoordinate (..),
    PageNum (..),
    ZoomMode (Zoom),
  )
import Hoodle.Type.Widget
  ( WidgetItem (PanZoomWidget),
    allWidgets,
    doesUsePanZoomWidget,
    panZoomWidgetConfig,
    panZoomWidgetPosition,
    panZoomWidgetTouchIsZoom,
    widgetConfig,
  )
import Hoodle.View.Coordinate
  ( CanvasGeometry
      ( canvas2Desktop,
        canvasDim,
        desktop2Canvas,
        desktop2Page,
        page2Desktop
      ),
    device2Desktop,
    makeCanvasGeometry,
  )
import Hoodle.View.Draw
  ( canvasImageSurface,
    doubleBufferFlush,
    drawWidgets,
    renderPanZoomWidget,
    virtualDoubleBufferDraw,
  )
import System.Process (readProcess)

--

data PanZoomMode = Moving | Zooming | Panning Bool

data PanZoomTouch = TouchMode | PenMode
  deriving (Show, Eq, Ord)

checkPointerInPanZoom ::
  (CanvasId, CanvasInfo a, CanvasGeometry) ->
  PointerCoord ->
  Maybe (Maybe (PanZoomMode, (CanvasCoordinate, CanvasCoordinate)))
checkPointerInPanZoom (_cid, cinfo, geometry) pcoord
  | b =
    let oxy@(CvsCoord (x, y)) = (desktop2Canvas geometry . device2Desktop geometry) pcoord
        owxy@(CvsCoord (x0, y0)) = view (canvasWidgets . panZoomWidgetConfig . panZoomWidgetPosition) cinfo
        obbox = BBox (x0, y0) (x0 + 100, y0 + 100)
        pbbox1 = BBox (x0 + 10, y0 + 10) (x0 + 50, y0 + 90)
        pbbox2 = BBox (x0 + 50, y0 + 10) (x0 + 90, y0 + 90)
        pbbox3 = BBox (x0, y0) (x0 + 10, y0 + 10)
        zbbox = BBox (x0 + 30, y0 + 30) (x0 + 70, y0 + 70)
     in if isPointInBBox obbox (x, y)
          then
            let mmode
                  | isPointInBBox zbbox (x, y) = Just (Zooming, (oxy, owxy))
                  | isPointInBBox pbbox1 (x, y) = Just (Panning False, (oxy, owxy))
                  | isPointInBBox pbbox2 (x, y) = Just (Panning True, (oxy, owxy))
                  | isPointInBBox pbbox3 (x, y) = Nothing
                  | otherwise = Just (Moving, (oxy, owxy))
             in Just mmode
          else Nothing
  | otherwise = Nothing
  where
    b = view (canvasWidgets . widgetConfig . doesUsePanZoomWidget) cinfo

-- |
startPanZoomWidget ::
  PanZoomTouch ->
  (CanvasId, CanvasInfo a, CanvasGeometry) ->
  Maybe (PanZoomMode, (CanvasCoordinate, CanvasCoordinate)) ->
  MainCoroutine ()
startPanZoomWidget tchmode (cid, cinfo, geometry) mmode = do
  modify (doesNotInvalidate .~ True)
  xst <- get
  cache <- renderCache
  let hdl = (getHoodle . view (unitHoodles . currentUnit)) xst
  case mmode of
    Nothing -> togglePanZoom cid
    Just (mode, (oxy, owxy)) -> do
      (srcsfc, Dim wsfc hsfc) <- case mode of
        Moving -> liftIO (canvasImageSurface cache cid Nothing geometry hdl)
        Zooming -> liftIO (canvasImageSurface cache cid (Just 1) geometry hdl)
        Panning _ -> liftIO (canvasImageSurface cache cid (Just 1) geometry hdl)
      -- need to draw other widgets here
      let otherwidgets = delete PanZoomWidget allWidgets
      liftIO $
        Cairo.renderWith
          srcsfc
          (drawWidgets otherwidgets hdl cinfo Nothing)
      -- end : need to draw other widgets here ^^^
      tgtsfc <-
        liftIO $
          Cairo.createImageSurface
            Cairo.FormatARGB32
            (floor wsfc)
            (floor hsfc)
      ctime <- liftIO getCurrentTime
      manipulatePZW (tchmode, mode) cid geometry (srcsfc, tgtsfc) owxy oxy ctime
      liftIO $ Cairo.surfaceFinish srcsfc
      liftIO $ Cairo.surfaceFinish tgtsfc
  modify (doesNotInvalidate .~ False)
  invalidateAll

-- |
findZoomXform ::
  Dimension ->
  ((Double, Double), (Double, Double), (Double, Double)) ->
  (Double, (Double, Double))
findZoomXform (Dim w h) ((xo, yo), (x0, _y0), (x, _y)) =
  let tx = x - x0
      ztx = 1 + tx / 200
      zx
        | ztx > 2 = 2
        | ztx < 0.5 = 0.5
        | otherwise = ztx
      z = zx
      xtrans = (1 - z) * xo / z - w
      ytrans = (1 - z) * yo / z - h
   in (z, (xtrans, ytrans))

-- |
findPanXform ::
  Dimension ->
  ((Double, Double), (Double, Double)) ->
  (Double, Double)
findPanXform (Dim w h) ((x0, y0), (x, y)) =
  let tx = x - x0
      ty = y - y0
      dx
        | tx > w = w
        | tx < (-w) = -w
        | otherwise = tx
      dy
        | ty > h = h
        | ty < (-h) = -h
        | otherwise = ty
   in (dx - w, dy - h)

-- | manipulate Pan-Zoom widget until released when grabbing the widget
manipulatePZW ::
  (PanZoomTouch, PanZoomMode) ->
  CanvasId ->
  CanvasGeometry ->
  -- | (Source, Target)
  (Cairo.Surface, Cairo.Surface) ->
  -- | original widget position
  CanvasCoordinate ->
  -- | where pen pressed
  CanvasCoordinate ->
  UTCTime ->
  MainCoroutine ()
manipulatePZW
  fullmode@(tchmode, mode)
  cid
  geometry
  (srcsfc, tgtsfc)
  owxy@(CvsCoord (xw, yw))
  oxy@(CvsCoord (x0, y0))
  otime = do
    r <- nextevent
    case r of
      PenMove _ pcoord -> if tchmode /= PenMode then again otime else moveact pcoord
      TouchMove _ pcoord ->
        if tchmode /= TouchMode
          then again otime
          else do
            b <- gets (view (settings . doesUseTouch))
            when b $ moveact pcoord
      PenUp _ pcoord -> if tchmode /= PenMode then again otime else upact pcoord
      TouchUp _ pcoord ->
        if tchmode /= TouchMode
          then again otime
          else do
            b <- gets (view (settings . doesUseTouch))
            when b $ upact pcoord
      _ -> again otime
    where
      again = manipulatePZW fullmode cid geometry (srcsfc, tgtsfc) owxy oxy
      moveact pcoord =
        processWithDefTimeInterval
          again
          ( \ctime ->
              movingRender mode cid geometry (srcsfc, tgtsfc) owxy oxy pcoord
                >> manipulatePZW fullmode cid geometry (srcsfc, tgtsfc) owxy oxy ctime
          )
          otime
      upact pcoord = do
        case mode of
          Zooming -> do
            let CvsCoord (x, y) = (desktop2Canvas geometry . device2Desktop geometry) pcoord
                CanvasDimension cdim = canvasDim geometry
                ccoord@(CvsCoord (xo, yo)) = CvsCoord (xw + 50, yw + 50)
                (z, (_, _)) = findZoomXform cdim ((xo, yo), (x0, y0), (x, y))
                nratio = zoomRatioFrmRelToCurr geometry z
                mpnpgxy = (desktop2Page geometry . canvas2Desktop geometry) ccoord
            canvasZoomUpdateGenRenderCvsId (return ()) cid (Just (Zoom nratio)) Nothing
            case mpnpgxy of
              Nothing -> return ()
              Just pnpgxy -> do
                uhdl <- gets (view (unitHoodles . currentUnit))
                geom' <- liftIO $ getCanvasGeometryCvsId cid uhdl
                let DeskCoord (xd, yd) = page2Desktop geom' pnpgxy
                    DeskCoord (xd0, yd0) = canvas2Desktop geom' ccoord
                moveViewPortBy (return ()) cid (\(xorig, yorig) -> (xorig + xd - xd0, yorig + yd - yd0))
          Panning _ -> do
            let (x_d, y_d) = (unDeskCoord . device2Desktop geometry) pcoord
                (x0_d, y0_d) =
                  (unDeskCoord . canvas2Desktop geometry)
                    (CvsCoord (x0, y0))
                (dx_d, dy_d) =
                  let (dx_d1, dy_d1) = (x_d - x0_d, y_d - y0_d)
                   in if
                          | abs dx_d1 < 25 -> (0, dy_d1)
                          | abs dy_d1 < 25 -> (dx_d1, 0)
                          | otherwise -> (dx_d1, dy_d1)
            moveViewPortBy (return ()) cid (\(xorig, yorig) -> (xorig - dx_d, yorig - dy_d))
          _ -> return ()
        invalidate cid

-- |
movingRender ::
  PanZoomMode ->
  CanvasId ->
  CanvasGeometry ->
  (Cairo.Surface, Cairo.Surface) ->
  CanvasCoordinate ->
  CanvasCoordinate ->
  PointerCoord ->
  MainCoroutine ()
movingRender mode cid geometry (srcsfc, tgtsfc) (CvsCoord (xw, yw)) (CvsCoord (x0, y0)) pcoord = do
  let CvsCoord (x, y) = (desktop2Canvas geometry . device2Desktop geometry) pcoord
  uhdl <- gets (view (unitHoodles . currentUnit))
  case mode of
    Moving -> do
      let CanvasDimension (Dim cw ch) = canvasDim geometry
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
          changeact = set (canvasWidgets . panZoomWidgetConfig . panZoomWidgetPosition) nwpos
          ncinfobox = (runIdentity . forBoth unboxBiXform (return . changeact)) cinfobox
          isTouchZoom = view (unboxLens (canvasWidgets . panZoomWidgetConfig . panZoomWidgetTouchIsZoom)) cinfobox
      pureUpdateUhdl $ setCanvasInfo (cid, ncinfobox)
      virtualDoubleBufferDraw srcsfc tgtsfc (return ()) (renderPanZoomWidget isTouchZoom Nothing nwpos)
    Zooming -> do
      let cinfobox = getCanvasInfo cid uhdl
      let pos = runIdentity (forBoth' unboxBiAct (return . view (canvasWidgets . panZoomWidgetConfig . panZoomWidgetPosition)) cinfobox)
      let (xo, yo) = (xw + 50, yw + 50)
          CanvasDimension cdim = canvasDim geometry
          (z, (xtrans, ytrans)) = findZoomXform cdim ((xo, yo), (x0, y0), (x, y))
          isTouchZoom = view (unboxLens (canvasWidgets . panZoomWidgetConfig . panZoomWidgetTouchIsZoom)) cinfobox
      virtualDoubleBufferDraw
        srcsfc
        tgtsfc
        (Cairo.save >> Cairo.scale z z >> Cairo.translate xtrans ytrans)
        (Cairo.restore >> renderPanZoomWidget isTouchZoom Nothing pos)
    Panning b -> do
      let cinfobox = getCanvasInfo cid uhdl
          CanvasDimension cdim = canvasDim geometry
          (xtrans, ytrans) = findPanXform cdim ((x0, y0), (x, y))
      let CanvasDimension (Dim cw ch) = canvasDim geometry
          nposx
            | xw + x - x0 < -50 = -50
            | xw + x - x0 > cw - 50 = cw - 50
            | otherwise = xw + x - x0
          nposy
            | yw + y - y0 < -50 = -50
            | yw + y - y0 > ch - 50 = ch - 50
            | otherwise = yw + y - y0
          nwpos =
            if b
              then CvsCoord (nposx, nposy)
              else runIdentity (forBoth' unboxBiAct (return . view (canvasWidgets . panZoomWidgetConfig . panZoomWidgetPosition)) cinfobox)
          ncinfobox = set (unboxLens (canvasWidgets . panZoomWidgetConfig . panZoomWidgetPosition)) nwpos cinfobox
          isTouchZoom = view (unboxLens (canvasWidgets . panZoomWidgetConfig . panZoomWidgetTouchIsZoom)) cinfobox
      pureUpdateUhdl $ setCanvasInfo (cid, ncinfobox)
      virtualDoubleBufferDraw
        srcsfc
        tgtsfc
        (Cairo.save >> Cairo.translate xtrans ytrans)
        (Cairo.restore >> renderPanZoomWidget isTouchZoom Nothing nwpos)
  --
  xst2 <- get
  let cinfobox = getCanvasInfo cid . view (unitHoodles . currentUnit) $ xst2
  liftIO $ forBoth' unboxBiAct (doubleBufferFlush tgtsfc) cinfobox

-- |
togglePanZoom :: CanvasId -> MainCoroutine ()
togglePanZoom cid = do
  pureUpdateUhdl $ \uhdl ->
    let cinfobox = getCanvasInfo cid uhdl
        ncinfobox = over (unboxLens (canvasWidgets . widgetConfig . doesUsePanZoomWidget)) not cinfobox
     in setCanvasInfo (cid, ncinfobox) uhdl
  invalidateInBBox Nothing Efficient cid

-- |
touchStart :: CanvasId -> PointerCoord -> MainCoroutine ()
touchStart cid pcoord = forBoth' unboxBiAct chk =<< gets (getCanvasInfo cid . view (unitHoodles . currentUnit))
  where
    chk :: CanvasInfo a -> MainCoroutine ()
    chk cinfo = do
      let cvs = view drawArea cinfo
          pnum = (PageNum . view currentPageNum) cinfo
          arr = view (viewInfo . pageArrangement) cinfo
      geometry <- liftIO $ makeCanvasGeometry pnum arr cvs
      let triplet = (cid, cinfo, geometry)
          oxy@(CvsCoord (x, y)) = (desktop2Canvas geometry . device2Desktop geometry) pcoord
          CvsCoord (x0, y0) = view (canvasWidgets . panZoomWidgetConfig . panZoomWidgetPosition) cinfo
          obbox = BBox (x0, y0) (x0 + 100, y0 + 100)
      xst <- get
      if isPointInBBox obbox (x, y)
        then do
          pureUpdateUhdl $ \uhdl ->
            let changeact :: CanvasInfo a -> CanvasInfo a
                changeact = over (canvasWidgets . panZoomWidgetConfig . panZoomWidgetTouchIsZoom) not
                ncinfobox = runIdentity . forBoth unboxBiXform (return . changeact) . getCanvasInfo cid $ uhdl
             in setCanvasInfo (cid, ncinfobox) uhdl
          invalidateInBBox Nothing Efficient cid
        else do
          let b = view (settings . doesUseTouch) xst
              isZoomTouch = view (canvasWidgets . panZoomWidgetConfig . panZoomWidgetTouchIsZoom) cinfo
          if b
            then
              if isZoomTouch
                then startPanZoomWidget TouchMode triplet (Just (Zooming, (oxy, oxy)))
                else startPanZoomWidget TouchMode triplet (Just (Panning False, (oxy, oxy)))
            else do
              let devlst = view deviceList xst
              doIOaction $ \_ -> do
                _ <- lensSetToggleUIForFlag "HANDA" (settings . doesUseTouch) xst
                -- ad hoc
                let touchstr = dev_touch_str devlst
                when (touchstr /= "touch") $
                  void $
                    readProcess
                      "xinput"
                      ["disable", dev_touch_str devlst]
                      ""
                --
                return (UsrEv ActionOrdered)
              void $ waitSomeEvent (\case TouchUp _ _ -> True; _ -> False)

toggleTouch :: MainCoroutine ()
toggleTouch = do
  _ <- updateFlagFromToggleUI "HANDA" (settings . doesUseTouch)
  xst <- get
  let devlst = view deviceList xst
      uhdl = view (unitHoodles . currentUnit) xst
      (cid, _cinfobox) = view currentCanvas uhdl
  when (view (settings . doesUseTouch) xst) $ do
    -- ad hoc
    let touchstr = dev_touch_str devlst
    when (touchstr /= "touch") $
      void $ liftIO $ readProcess "xinput" ["enable", dev_touch_str devlst] ""
    --
    pureUpdateUhdl $
      ( currentCanvasInfo
          . unboxLens (canvasWidgets . widgetConfig . doesUsePanZoomWidget)
      )
        .~ True
    invalidateInBBox Nothing Efficient cid
    return ()
