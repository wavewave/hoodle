-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Widget.PanZoom
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Pan-Zoom widget drawing and action
-- 
-----------------------------------------------------------------------------

module Hoodle.Widget.PanZoom where

-- from other packages
import           Control.Lens (view,set,over)
import           Control.Monad.Identity 
import           Control.Monad.State 
import           Data.Time.Clock 
import           Graphics.Rendering.Cairo 
-- import           Graphics.UI.Gtk hiding (get,set) 
-- from hoodle-platform 
import           Data.Hoodle.BBox
import           Data.Hoodle.Simple
import           Graphics.Hoodle.Render.Util.HitTest
-- from this package 
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Page
import           Hoodle.Coroutine.Pen 
import           Hoodle.Coroutine.Scroll
import           Hoodle.Device
import           Hoodle.ModelAction.Page 
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState 
import           Hoodle.Type.PageArrangement 
import           Hoodle.Type.Widget
import           Hoodle.View.Coordinate
import           Hoodle.View.Draw
-- 

data PanZoomMode = Moving | Zooming | Panning Bool

checkPointerInPanZoom :: (ViewMode a) => 
                         (CanvasId,CanvasInfo a,CanvasGeometry) 
                      -> PointerCoord 
                      -> Maybe (Maybe (PanZoomMode,(CanvasCoordinate,CanvasCoordinate)))
checkPointerInPanZoom (cid,cinfo,geometry) pcoord =  
    let oxy@(CvsCoord (x,y)) = (desktop2Canvas geometry . device2Desktop geometry) pcoord
        owxy@(CvsCoord (x0,y0)) = view (canvasWidgets.panZoomWidgetPosition) cinfo
        obbox = BBox (x0,y0) (x0+100,y0+100) 
        pbbox1 = BBox (x0+10,y0+10) (x0+50,y0+90)
        pbbox2 = BBox (x0+50,y0+10) (x0+90,y0+90)
        pbbox3 = BBox (x0+90,y0) (x0+100,y0+10)
        zbbox = BBox (x0+30,y0+30) (x0+70,y0+70)
    in if (isPointInBBox obbox (x,y))  
       then let mmode | isPointInBBox zbbox (x,y) = Just (Zooming,(oxy,owxy))
                   | isPointInBBox pbbox1 (x,y) = Just (Panning False,(oxy,owxy))
                   | isPointInBBox pbbox2 (x,y) = Just (Panning True,(oxy,owxy))
                   | isPointInBBox pbbox3 (x,y) = Nothing
                   | otherwise = Just (Moving,(oxy,owxy))
            in (Just mmode)
       else Nothing   
            
-- | 
startPanZoomWidget :: (ViewMode a) =>
                      (CanvasId,CanvasInfo a,CanvasGeometry)
                   -> Maybe (PanZoomMode,(CanvasCoordinate,CanvasCoordinate))
                   -> MainCoroutine ()
startPanZoomWidget (cid,cinfo,geometry) mmode = do 
    xst <- get 
    let hdl = getHoodle xst
    case mmode of 
      Nothing -> togglePanZoom
      Just (mode,(oxy,owxy)) -> do 
        (sfc,Dim wsfc hsfc) <- case mode of 
                                 Moving -> liftIO (canvasImageSurface Nothing geometry hdl)
                                 Zooming -> liftIO (canvasImageSurface (Just 1) geometry hdl)
                                 Panning _ -> liftIO (canvasImageSurface (Just 1) geometry hdl) 
        sfc2 <- liftIO $ createImageSurface FormatARGB32 (floor wsfc) (floor hsfc)
        ctime <- liftIO getCurrentTime 
        startWidgetAction mode cid geometry (sfc,sfc2) owxy oxy ctime 
        liftIO $ surfaceFinish sfc 
        liftIO $ surfaceFinish sfc2




-- | 
findZoomXform :: Dimension 
               -> ((Double,Double),(Double,Double),(Double,Double)) 
               -> (Double,(Double,Double))
findZoomXform (Dim w h) ((xo,yo),(x0,y0),(x,y)) = 
    let tx = x - x0 
        ty = y - y0 
        ztx = 1 + tx / 200
        zty = 1 + ty / 200
        zx | ztx > 2 = 2  
           | ztx < 0.5 = 0.5
           | otherwise = ztx
        zy | zty > 2 = 2  
           | zty < 0.5 = 0.5
           | otherwise = zty                                          
        z | zx >= 1 && zy >= 1 = max zx zy
          | zx < 1 && zy < 1 = min zx zy 
          | otherwise = zx
        xtrans = (1 -z)*xo/z-w
        ytrans = (1- z)*yo/z-h 
    in (z,(xtrans,ytrans))

-- |
findPanXform :: Dimension 
             -> ((Double,Double),(Double,Double)) 
             -> (Double,Double)
findPanXform (Dim w h) ((x0,y0),(x,y)) = 
    let tx = x - x0 
        ty = y - y0 
        dx | tx > w = w 
           | tx < (-w) = -w 
           | otherwise = tx 
        dy | ty > h = h 
           | ty < (-h) = -h 
           | otherwise = ty 
    in ((dx-w),(dy-h))

-- | 
startWidgetAction :: PanZoomMode 
                     -> CanvasId 
                     -> CanvasGeometry 
                     -> (Surface,Surface)
                     -> CanvasCoordinate -- ^ original widget position
                     -> CanvasCoordinate -- ^ where pen pressed 
                     -> UTCTime
                     -> MainCoroutine ()
startWidgetAction mode cid geometry (sfc,sfc2)
                  owxy@(CvsCoord (xw,yw)) oxy@(CvsCoord (x0,y0)) otime = do
  r <- nextevent
  case r of 
    PenMove _ pcoord -> do 
      processWithDefTimeInterval
        (startWidgetAction mode cid geometry (sfc,sfc2) owxy oxy) 
        (\ctime -> movingRender mode cid geometry (sfc,sfc2) owxy oxy pcoord 
                   >> startWidgetAction mode cid geometry (sfc,sfc2) owxy oxy ctime)
        otime 
    PenUp _ pcoord -> do 
      case mode of 
        Zooming -> do 
          let CvsCoord (x,y) = (desktop2Canvas geometry . device2Desktop geometry) pcoord 
              CanvasDimension cdim = canvasDim geometry 
              ccoord@(CvsCoord (xo,yo)) = CvsCoord (xw+50,yw+50)
              (z,(_,_)) = findZoomXform cdim ((xo,yo),(x0,y0),(x,y))
              nratio = zoomRatioFrmRelToCurr geometry z
              mpnpgxy = (desktop2Page geometry . canvas2Desktop geometry) ccoord 
          canvasZoomUpdateGenRenderCvsId (return ()) cid (Just (Zoom nratio)) Nothing 
          case mpnpgxy of 
            Nothing -> return () 
            Just pnpgxy -> do 
              xstate <- get
              geom' <- liftIO $ getCanvasGeometryCvsId cid xstate
              let DeskCoord (xd,yd) = page2Desktop geom' pnpgxy
                  DeskCoord (xd0,yd0) = canvas2Desktop geom' ccoord 
              moveViewPortBy (return ()) cid 
                (\(xorig,yorig)->(xorig+xd-xd0,yorig+yd-yd0)) 
        Panning _ -> do 
          let (x_d,y_d) = (unDeskCoord . device2Desktop geometry) pcoord  
              (x0_d,y0_d) = (unDeskCoord . canvas2Desktop geometry) 
                              (CvsCoord (x0,y0))
              (dx_d,dy_d) = (x_d-x0_d,y_d-y0_d)
          moveViewPortBy (return ()) cid 
            (\(xorig,yorig)->(xorig-dx_d,yorig-dy_d))                 
        _ -> return ()
      invalidate cid 
    _ -> startWidgetAction mode cid geometry (sfc,sfc2) owxy oxy otime

-- | 
movingRender :: PanZoomMode -> CanvasId -> CanvasGeometry -> (Surface,Surface) 
                -> CanvasCoordinate -> CanvasCoordinate -> PointerCoord 
                -> MainCoroutine () 
movingRender mode cid geometry (sfc,sfc2) (CvsCoord (xw,yw)) (CvsCoord (x0,y0)) pcoord = do 
          let CvsCoord (x,y) = (desktop2Canvas geometry . device2Desktop geometry) pcoord 
          xst <- get 
          case mode of
            Moving -> do 
              let CanvasDimension (Dim cw ch) = canvasDim geometry 
                  cinfobox = getCanvasInfo cid xst 
                  nposx | xw+x-x0 < -50 = -50 
                        | xw+x-x0 > cw-50 = cw-50 
                        | otherwise = xw+x-x0
                  nposy | yw+y-y0 < -50 = -50 
                        | yw+y-y0 > ch-50 = ch-50 
                        | otherwise = yw+y-y0                             
                  nwpos = CvsCoord (nposx,nposy) 
                  changeact :: (ViewMode a) => CanvasInfo a -> CanvasInfo a 
                  changeact cinfo =  
                    set (canvasWidgets.panZoomWidgetPosition) nwpos $ cinfo
                  ncinfobox = selectBox changeact changeact  cinfobox
              put (setCanvasInfo (cid,ncinfobox) xst)
              virtualDoubleBufferDraw sfc sfc2 (return ()) (renderPanZoomWidget Nothing nwpos)
            Zooming -> do 
              let cinfobox = getCanvasInfo cid xst               
              let pos = runIdentity (boxAction (return.view (canvasWidgets.panZoomWidgetPosition)) cinfobox)
              let (xo,yo) = (xw+50,yw+50)
                  CanvasDimension cdim = canvasDim geometry 
                  (z,(xtrans,ytrans)) = findZoomXform cdim ((xo,yo),(x0,y0),(x,y))
              virtualDoubleBufferDraw sfc sfc2 
                (save >> scale z z >> translate xtrans ytrans)
                (restore >> renderPanZoomWidget Nothing pos)
            Panning b -> do 
              let cinfobox = getCanvasInfo cid xst               
                  CanvasDimension cdim = canvasDim geometry 
                  (xtrans,ytrans) = findPanXform cdim ((x0,y0),(x,y))
              let CanvasDimension (Dim cw ch) = canvasDim geometry 
                  nposx | xw+x-x0 < -50 = -50 
                        | xw+x-x0 > cw-50 = cw-50 
                        | otherwise = xw+x-x0
                  nposy | yw+y-y0 < -50 = -50 
                        | yw+y-y0 > ch-50 = ch-50 
                        | otherwise = yw+y-y0                             
                  nwpos = if b 
                          then CvsCoord (nposx,nposy) 
                          else 
                            runIdentity (boxAction (return.view (canvasWidgets.panZoomWidgetPosition)) cinfobox)
                  ncinfobox = set (unboxLens (canvasWidgets.panZoomWidgetPosition)) nwpos cinfobox
              put (setCanvasInfo (cid,ncinfobox) xst)
              virtualDoubleBufferDraw sfc sfc2 
                (save >> translate xtrans ytrans) 
                (restore >> renderPanZoomWidget Nothing nwpos)
          --   
          xst2 <- get 
          let cinfobox = getCanvasInfo cid xst2 
          liftIO $ boxAction (doubleBufferFlush sfc2) cinfobox

  
-- | 
togglePanZoom :: MainCoroutine () 
togglePanZoom = do 
  modify (over (currentCanvasInfo . unboxLens (canvasWidgets.widgetConfig.doesUsePanZoomWidget)) not)
  invalidateAll  
