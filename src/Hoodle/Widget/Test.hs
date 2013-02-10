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
import           Data.Hoodle.Zipper
import           Graphics.Hoodle.Render.Type
import           Graphics.Hoodle.Render.Util
import           Graphics.Hoodle.Render.Util.HitTest
-- 
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Select
import           Hoodle.Device
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


data WidgetMode = Moving | Zooming

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
          ibbox = BBox (x0+30,y0+30) (x0+70,y0+70)
              
          
      if (isPointInBBox obbox (x,y))  
         then do 
           ctime <- liftIO getCurrentTime 
           let mode = case  isPointInBBox ibbox (x,y) of 
                        True -> Zooming 
                        False -> Moving 
           let hdl = getHoodle xst
           (sfc,sfc2) <- liftIO (canvasImageSurface geometry hdl)
           
           startWidgetAction mode cid geometry (sfc,sfc2) owxy oxy ctime 
           liftIO $ surfaceFinish sfc 
           liftIO $ surfaceFinish sfc2
         else act 
widgetCheckPen cid pcoord act = act  


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
      if willUpdate  
        then do 
          let CvsCoord (x,y) = (desktop2Canvas geometry . device2Desktop geometry) pcoord 
          xst <- get 
          
          case mode of
            Moving -> do 
              let cinfobox = getCanvasInfo cid xst 
                  nwpos = CvsCoord (xw+x-x0,yw+y-y0)
              
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
              
            Zooming -> do -- return () 
              let cinfobox = getCanvasInfo cid xst               
              let pos = runIdentity (boxAction (return . view (canvasWidgets.testWidgetPosition)) cinfobox )
              let (xo,yo) = (xw+50,yw+50)
              let tx = if x0 > xo then x - x0 else x0 - x 
                  ty = if y0 > yo then y - y0 else y0 - y
                  ztx = 1 + tx / 200
                  zty = 1 + ty / 200
                  zx | ztx > 5 = 5  
                     | ztx < 0.2 = 0.2
                     | otherwise = ztx
                  zy | zty > 5 = 5  
                     | zty < 0.2 = 0.2
                     | otherwise = zty                                          
                  z | zx >= 1 && zy >= 1 = max zx zy
                    | zx < 1 && zy < 1 = min zx zy 
                    | otherwise = zx
              renderWith sfc2 $ do 
                  save
                  scale z z
                  setSourceSurface sfc 0 0 
                  setOperator OperatorSource 
                  paint
                  setOperator OperatorOver
                  restore
                  renderTestWidget Nothing pos 
              
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

          -- liftIO $ putStrLn $ "(w,h) =" ++ show (w,h)
          liftIO $ boxAction drawact cinfobox
          -- invalidateInBBox Nothing Efficient cid 
            
            
          startWidgetAction mode cid geometry (sfc,sfc2) owxy oxy ctime
        else      
          startWidgetAction mode cid geometry (sfc,sfc2) owxy oxy otime
    PenUp _ pcoord -> invalidateAll -- return () 
    _ -> startWidgetAction mode cid geometry (sfc,sfc2) owxy oxy otime

