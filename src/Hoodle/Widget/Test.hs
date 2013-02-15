-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Widget.Test
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Widget.Test where

-- from other packages
import           Control.Category
import           Control.Lens (view,set,over)
import           Control.Monad.Identity 
import           Control.Monad.State 
import           Data.Time.Clock 
import           Graphics.Rendering.Cairo 
import           Graphics.UI.Gtk hiding (get,set) 
-- import           Graphics.UI.Gtk hiding (get,set)
-- import qualified Graphics.UI.Gtk as Gtk (get)
-- from hoodle-platform 
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple
import           Data.Hoodle.Zipper
import           Graphics.Hoodle.Render.Type
import           Graphics.Hoodle.Render.Util
import           Graphics.Hoodle.Render.Util.HitTest
-- 
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Page
import           Hoodle.Coroutine.Scroll
import           Hoodle.Coroutine.Select
import           Hoodle.Device
import           Hoodle.ModelAction.Page 
import           Hoodle.ModelAction.Select
import           Hoodle.Type.Alias
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState 
import           Hoodle.Type.PageArrangement 
import           Hoodle.Type.Predefined 
import           Hoodle.View.Coordinate
import           Hoodle.View.Draw
-- 
import Prelude hiding ((.),id)


data WidgetMode = Moving | Zooming | Panning Bool 

widgetCheckPen :: CanvasId -> PointerCoord 
               -> MainCoroutine () 
               -> MainCoroutine ()
widgetCheckPen cid pcoord act = do 
    xst <- get
    let cinfobox = getCanvasInfo cid xst 
    boxAction (f xst) cinfobox 
  where 
    f xst cinfo = do 
      let cvs = view drawArea cinfo
          pnum = (PageNum . view currentPageNum) cinfo 
          arr = view (viewInfo.pageArrangement) cinfo
      geometry <- liftIO $ makeCanvasGeometry pnum arr cvs 
      let oxy@(CvsCoord (x,y)) = (desktop2Canvas geometry . device2Desktop geometry) pcoord
      let owxy@(CvsCoord (x0,y0)) = view (canvasWidgets.testWidgetPosition) cinfo
          obbox = BBox (x0,y0) (x0+100,y0+100) 
          pbbox1 = BBox (x0+10,y0+10) (x0+50,y0+90)
          pbbox2 = BBox (x0+50,y0+10) (x0+90,y0+90)
          zbbox = BBox (x0+30,y0+30) (x0+70,y0+70)
              
          
      if (isPointInBBox obbox (x,y))  
         then do 
           ctime <- liftIO getCurrentTime 
           let mode | isPointInBBox zbbox (x,y) = Zooming 
                    | isPointInBBox pbbox1 (x,y) = Panning False
                    | isPointInBBox pbbox2 (x,y) = Panning True 
                    | otherwise = Moving 
           let hdl = getHoodle xst
           (sfc,Dim wsfc hsfc) <- 
             case mode of 
               Moving -> liftIO (canvasImageSurface Nothing geometry hdl)
               Zooming -> liftIO (canvasImageSurface (Just 1) geometry hdl)
               Panning _ -> liftIO (canvasImageSurface (Just 1) geometry hdl) 
           sfc2 <- liftIO $ createImageSurface FormatARGB32 (floor wsfc) (floor hsfc)
           
           startWidgetAction mode cid geometry (sfc,sfc2) owxy oxy ctime 
           liftIO $ surfaceFinish sfc 
           liftIO $ surfaceFinish sfc2
         else do 
           act 


findZoomXform :: Dimension 
               -> ((Double,Double),(Double,Double),(Double,Double)) 
               -> (Double,(Double,Double))
findZoomXform (Dim w h) ((xo,yo),(x0,y0),(x,y)) = 
    let tx = x - x0 --  if x0 > xo then x - x0 else x0 - x 
        ty = y - y0 -- if y0 > yo then y - y0 else y0 - y
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
startWidgetAction :: WidgetMode 
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
      ctime <- liftIO getCurrentTime 
      let dtime = diffUTCTime ctime otime 
          willUpdate = dtime > dtime_bound 
      when willUpdate $ 
        movingRender mode cid geometry (sfc,sfc2) owxy oxy pcoord      
      if willUpdate
        then 
          startWidgetAction mode cid geometry (sfc,sfc2) owxy oxy ctime
        else      
          startWidgetAction mode cid geometry (sfc,sfc2) owxy oxy otime
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
              geometry <- liftIO $ getCanvasGeometryCvsId cid xstate
              let DeskCoord (xd,yd) = page2Desktop geometry pnpgxy
                  DeskCoord (xd0,yd0) = canvas2Desktop geometry ccoord 
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


movingRender mode cid geometry (sfc,sfc2) owxy@(CvsCoord (xw,yw)) oxy@(CvsCoord (x0,y0)) pcoord = do 
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
                  nwpos = CvsCoord (nposx,nposy) -- (xw+x-x0,yw+y-y0)
                  changeact :: (ViewMode a) => CanvasInfo a -> CanvasInfo a 
                  changeact cinfo =  
                    set (canvasWidgets.testWidgetPosition) nwpos $ cinfo
                  ncinfobox = selectBox changeact changeact  cinfobox
              put (setCanvasInfo (cid,ncinfobox) xst)
              renderWith sfc2 $ do 
                setSourceSurface sfc 0 0 
                setOperator OperatorSource 
                paint
                setOperator OperatorOver
                renderTestWidget Nothing nwpos 
            Zooming -> do 
              let cinfobox = getCanvasInfo cid xst               
              let pos = runIdentity (boxAction (return . view (canvasWidgets.testWidgetPosition)) cinfobox )
              let (xo,yo) = (xw+50,yw+50)
                  CanvasDimension cdim = canvasDim geometry 
                  (z,(xtrans,ytrans)) = findZoomXform cdim ((xo,yo),(x0,y0),(x,y))
              renderWith sfc2 $ do 
                  save
                  scale z z
                  translate xtrans ytrans 
                  setSourceSurface sfc 0 0 
                  setOperator OperatorSource 
                  paint
                  setOperator OperatorOver
                  restore
                  renderTestWidget Nothing pos 
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
                            runIdentity (boxAction (return . view (canvasWidgets.testWidgetPosition)) cinfobox)
                  changeact :: (ViewMode a) => CanvasInfo a -> CanvasInfo a 
                  changeact cinfo =  
                    set (canvasWidgets.testWidgetPosition) nwpos $ cinfo
                  ncinfobox = selectBox changeact changeact  cinfobox
              put (setCanvasInfo (cid,ncinfobox) xst)
                  
              renderWith sfc2 $ do 
                  save
                  translate xtrans ytrans 
                  setSourceSurface sfc 0 0 
                  setOperator OperatorSource 
                  paint
                  setOperator OperatorOver
                  restore
                  renderTestWidget Nothing nwpos 
          --   
          xst2 <- get 
          let cinfobox = getCanvasInfo cid xst2 
              drawact :: (ViewMode a) => CanvasInfo a -> IO ()
              drawact cinfo = do 
                let canvas = view drawArea cinfo 
                    pos = view (canvasWidgets.testWidgetPosition) cinfo
                win <- widgetGetDrawWindow canvas
                renderWithDrawable win $ do 
                  setSourceSurface sfc2 0 0 
                  setOperator OperatorSource 
                  paint
          liftIO $ boxAction drawact cinfobox
