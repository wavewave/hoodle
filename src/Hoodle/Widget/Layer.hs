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
import           Data.List (delete)
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
                    -> Maybe (Maybe (CanvasCoordinate,CanvasCoordinate))
checkPointerInLayer (cid,cinfo,geometry) pcoord 
  | b =  
    let oxy@(CvsCoord (x,y)) = (desktop2Canvas geometry . device2Desktop geometry) pcoord
        owxy@(CvsCoord (x0,y0)) = view (canvasWidgets.layerWidgetPosition) cinfo
        obbox = BBox (x0,y0) (x0+100,y0+100) 
        closebbox = BBox (x0,y0) (x0+10,y0+10)
        r | isPointInBBox closebbox (x,y) = Just Nothing 
          | isPointInBBox obbox (x,y)     = Just (Just (oxy,owxy))
          | otherwise                     = Nothing   
    in r
  | otherwise = Nothing 
  where b = view (canvasWidgets.widgetConfig.doesUseLayerWidget) cinfo 

startLayerWidget :: ViewMode a =>
                    (CanvasId,CanvasInfo a,CanvasGeometry)
                 -> Maybe (CanvasCoordinate,CanvasCoordinate)
                 -> MainCoroutine () 
startLayerWidget (cid,cinfo,geometry) Nothing = toggleLayer
startLayerWidget (cid,cinfo,geometry) (Just (oxy,owxy)) = do 
    xst <- get 
    let hdl = getHoodle xst
    (srcsfc,Dim wsfc hsfc) <- liftIO (canvasImageSurface Nothing geometry hdl)
    -- need to draw other widgets here                             
    let otherwidgets = delete LayerWidget allWidgets 
    liftIO $ renderWith srcsfc (drawWidgets otherwidgets hdl cinfo Nothing) 
    -- end : need to draw other widgets here ^^^
    tgtsfc <- liftIO $ createImageSurface FormatARGB32 (floor wsfc) (floor hsfc)
    ctime <- liftIO getCurrentTime 
    let CvsCoord (x0,y0) = owxy 
        CvsCoord (x,y) = oxy 
        act 
          | hitLassoPoint (fromList [(x0+80,y0),(x0+100,y0),(x0+100,y0+20)]) (x,y) = gotoNextLayer
          | hitLassoPoint (fromList [(x0,y0+80),(x0,y0+100),(x0+20,y0+100)]) (x,y) = gotoPrevLayer 
          | otherwise = manipulateLW cid geometry (srcsfc,tgtsfc) owxy oxy ctime 
    act
    liftIO $ surfaceFinish srcsfc 
    liftIO $ surfaceFinish tgtsfc
  
-- | main event loop for layer widget
manipulateLW :: CanvasId 
             -> CanvasGeometry 
             -> (Surface,Surface) 
             -> CanvasCoordinate 
             -> CanvasCoordinate 
             -> UTCTime 
             -> MainCoroutine () 
manipulateLW cid geometry (srcsfc,tgtsfc) 
             owxy@(CvsCoord (xw,yw)) oxy@(CvsCoord (x0,y0)) otime = do 
    r <- nextevent
    case r of 
      PenMove _ pcoord -> do 
        processWithDefTimeInterval
          (manipulateLW cid geometry (srcsfc,tgtsfc) owxy oxy) 
          (\ctime -> moveLayerWidget cid geometry (srcsfc,tgtsfc) owxy oxy pcoord 
                     >> manipulateLW cid geometry (srcsfc,tgtsfc) owxy oxy ctime)
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
moveLayerWidget cid geometry (srcsfc,tgtsfc) (CvsCoord (xw,yw)) (CvsCoord (x0,y0)) pcoord = do 
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
    liftIO $ boxAction (\cinfo-> virtualDoubleBufferDraw srcsfc tgtsfc (return ()) 
                                    (drawLayerWidget hdl cinfo Nothing nwpos) 
                                 >> doubleBufferFlush tgtsfc cinfo) cinfobox
  
-- | 
toggleLayer :: MainCoroutine () 
toggleLayer = do 
  modify (over (currentCanvasInfo . unboxLens (canvasWidgets.widgetConfig.doesUseLayerWidget)) not)
  invalidateAll  
