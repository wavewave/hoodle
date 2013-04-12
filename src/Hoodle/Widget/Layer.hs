-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Widget.Layer
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Layer widget
-- 
-----------------------------------------------------------------------------

module Hoodle.Widget.Layer where

import Control.Lens (view,set,over)
import Control.Monad.State 
import Control.Monad.Trans
import           Data.Sequence
import           Data.Time
import           Graphics.Rendering.Cairo
--
import           Data.Hoodle.BBox
import           Data.Hoodle.Simple 
import           Graphics.Hoodle.Render.Util.HitTest
--
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Layer
import           Hoodle.Coroutine.Pen
import           Hoodle.Device
import           Hoodle.ModelAction.Select
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState 
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.Widget
import           Hoodle.View.Coordinate
import           Hoodle.View.Draw


checkPointerInLayer :: ViewMode a => 
                       (CanvasId,CanvasInfo a,CanvasGeometry) 
                    -> PointerCoord 
                    -> Maybe (CanvasCoordinate,CanvasCoordinate)
checkPointerInLayer (cid,cinfo,geometry) pcoord 
  | b =  
    let oxy@(CvsCoord (x,y)) = (desktop2Canvas geometry . device2Desktop geometry) pcoord
        owxy@(CvsCoord (x0,y0)) = view (canvasWidgets.layerWidgetPosition) cinfo
        obbox = BBox (x0,y0) (x0+100,y0+100) 
    in if (isPointInBBox obbox (x,y))  
       then Just (oxy,owxy)
       else Nothing   
  | otherwise = Nothing 
  where b = view (canvasWidgets.widgetConfig.doesUseLayerWidget) cinfo 

startLayerWidget :: ViewMode a =>
                    (CanvasId,CanvasInfo a,CanvasGeometry)
                 -> (CanvasCoordinate,CanvasCoordinate)
                 -> MainCoroutine () 
startLayerWidget (cid,cinfo,geometry) (oxy,owxy) = do 
    xst <- get 
    let hdl = getHoodle xst
    (sfc,Dim wsfc hsfc) <- liftIO (canvasImageSurface Nothing geometry hdl)
    sfc2 <- liftIO $ createImageSurface FormatARGB32 (floor wsfc) (floor hsfc)
    ctime <- liftIO getCurrentTime 
    let CvsCoord (x0,y0) = owxy 
        CvsCoord (x,y) = oxy 
        act 
          | hitLassoPoint (fromList [(x0+80,y0),(x0+100,y0),(x0+100,y0+20)]) (x,y) = gotoNextLayer
          | hitLassoPoint (fromList [(x0,y0+80),(x0,y0+100),(x0+20,y0+100)]) (x,y) = gotoPrevLayer 
          | otherwise = layerWidgetEventLoop cid geometry (sfc,sfc2) owxy oxy ctime 
    act
    liftIO $ surfaceFinish sfc 
    liftIO $ surfaceFinish sfc2
  
layerWidgetEventLoop :: CanvasId 
                     -> CanvasGeometry 
                     -> (Surface,Surface) 
                     -> CanvasCoordinate 
                     -> CanvasCoordinate 
                     -> UTCTime 
                     -> MainCoroutine () 
layerWidgetEventLoop cid geometry (sfc,sfc2) 
                     owxy@(CvsCoord (xw,yw)) oxy@(CvsCoord (x0,y0)) otime = do 
    r <- nextevent
    case r of 
      PenMove _ pcoord -> do 
        processWithDefTimeInterval
          (layerWidgetEventLoop cid geometry (sfc,sfc2) owxy oxy) 
          (\ctime -> moveLayerWidget cid geometry (sfc,sfc2) owxy oxy pcoord 
                     >> layerWidgetEventLoop cid geometry (sfc,sfc2) owxy oxy ctime)
          otime 
      PenUp _ pcoord -> invalidate cid 
      _ -> return ()

moveLayerWidget :: CanvasId 
                   -> CanvasGeometry 
                   -> (Surface,Surface) 
                   -> CanvasCoordinate 
                   -> CanvasCoordinate 
                   -> PointerCoord
                   -> MainCoroutine ()
moveLayerWidget cid geometry (sfc,sfc2) (CvsCoord (xw,yw)) (CvsCoord (x0,y0)) pcoord = do 
    let CvsCoord (x,y) = (desktop2Canvas geometry . device2Desktop geometry) pcoord 
    xst <- get 
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
          set (canvasWidgets.layerWidgetPosition) nwpos $ cinfo
        ncinfobox = selectBox changeact changeact  cinfobox
    put (setCanvasInfo (cid,ncinfobox) xst)
    let hdl = getHoodle xst 

    -- 
    xst2 <- get 
    let cinfobox = getCanvasInfo cid xst2 
    liftIO $ boxAction (\cinfo-> virtualDoubleBufferDraw sfc sfc2 (return ()) (drawLayerWidget hdl cinfo Nothing nwpos) >> doubleBufferFlush sfc2 cinfo) cinfobox
  
  