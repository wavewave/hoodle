{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.View.Coordinate
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.View.Coordinate where 

import Graphics.UI.Gtk hiding (get,set)
import Control.Applicative
import Control.Category
import Data.Label 
import Prelude hiding ((.),id)
import Data.Xournal.Simple (Dimension(..))
import Data.Xournal.Generic
import Data.Xournal.BBox (BBox(..))
import Application.HXournal.Device
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.PageArrangement

-- | data structure for transformation among screen, canvas, desktop and page coordinates

newtype ScreenCoordinate = ScrCoord { unScrCoord :: (Double,Double) } 
newtype CanvasCoordinate = CvsCoord { unCvsCoord :: (Double,Double) }
newtype DesktopCoordinate = DeskCoord { unDeskCoord :: (Double,Double) } 
newtype PageCoordinate = PageCoord { unPageCoord :: (Double,Double) } 
newtype PageNum = PageNum { unPageNum :: Int } 

data CanvasGeometry = 
  CanvasGeometry 
  { screenDim :: ScreenDimension
  , canvasDim :: CanvasDimension
  -- , canvasOrigin :: CanvasOrigin 
  , desktopDim :: DesktopDimension 
  -- , canvasViewPort :: ViewPortBBox -- ^ in desktop coordinate 
  , screen2Canvas :: ScreenCoordinate -> CanvasCoordinate
  , canvas2Screen :: CanvasCoordinate -> ScreenCoordinate
  , canvas2Desktop :: CanvasCoordinate -> DesktopCoordinate
  , desktop2Canvas :: DesktopCoordinate -> CanvasCoordinate
  , desktop2Page :: DesktopCoordinate -> Maybe (PageNum,PageCoordinate)
  , page2Desktop :: (PageNum,PageCoordinate) -> DesktopCoordinate
  } 

-- | make a canvas geometry data structure from current status 

makeCanvasGeometry :: (PageNum, GPage b s a) 
                      -> ZoomMode 
                      -> PageArrangement vmode 
                      -> DrawingArea 
                      -> IO CanvasGeometry 
makeCanvasGeometry (cpn,page) zmode arr canvas = do 
  win <- widgetGetDrawWindow canvas
  (w',h') <- return . ((,) <$> fromIntegral.fst <*> fromIntegral.snd) =<< widgetGetSize canvas
  screen <- widgetGetScreen canvas
  (ws,hs) <- (,) <$> (fromIntegral <$> screenGetWidth screen)
                 <*> (fromIntegral <$> screenGetHeight screen)
  (x0,y0) <- return . ((,) <$> fromIntegral.fst <*> fromIntegral.snd ) =<< drawWindowGetOrigin win
  let (Dim w h) = get g_dimension page
      cdim = CanvasDimension (Dim w' h')
      corig = CanvasOrigin (x0,y0)
  let (deskdim, cvsvbbox, p2d, d2p) = 
        case arr of  
          SingleArrangement pdim vbbox -> ( DesktopDimension . unPageDimension $ pdim
                                          , vbbox
                                          , DeskCoord . unPageCoord . snd
                                          , \(DeskCoord coord) ->Just (cpn,(PageCoord coord)) )
          _ -> error "not implemented yet for ContinuousArrangement in getCanvasGeometry"
  let s2c = xformScreen2Canvas corig
      c2s = xformCanvas2Screen corig
      c2d = xformCanvas2Desk cdim cvsvbbox 
      d2c = xformDesk2Canvas cdim cvsvbbox
  return $ CanvasGeometry (ScreenDimension (Dim ws hs)) (CanvasDimension (Dim w' h')) 
                          deskdim s2c c2s c2d d2c d2p p2d
    
    
xformScreen2Canvas :: CanvasOrigin -> ScreenCoordinate -> CanvasCoordinate
xformScreen2Canvas (CanvasOrigin (x0,y0)) (ScrCoord (sx,sy)) = CvsCoord (sx-x0,sy-y0)

xformCanvas2Screen :: CanvasOrigin -> CanvasCoordinate -> ScreenCoordinate 
xformCanvas2Screen (CanvasOrigin (x0,y0)) (CvsCoord (cx,cy)) = ScrCoord (cx+x0,cy+y0)

xformCanvas2Desk :: CanvasDimension -> ViewPortBBox -> CanvasCoordinate -> DesktopCoordinate 
xformCanvas2Desk (CanvasDimension (Dim w h)) (ViewPortBBox (BBox (x1,y1) (x2,y2))) 
                 (CvsCoord (cx,cy)) = DeskCoord (cx*(x2-x1)/w+x1,cy*(y2-y1)/h+x2) 

xformDesk2Canvas :: CanvasDimension -> ViewPortBBox -> DesktopCoordinate -> CanvasCoordinate
xformDesk2Canvas (CanvasDimension (Dim w h)) (ViewPortBBox (BBox (x1,y1) (x2,y2)))
                 (DeskCoord (dx,dy)) = CvsCoord ((dx-x1)*w/(x2-x1),(dy-y1)*h/(y2-y1))
                                       
screen2Desktop :: CanvasGeometry -> ScreenCoordinate -> DesktopCoordinate
screen2Desktop geometry = canvas2Desktop geometry . screen2Canvas geometry  

desktop2Screen :: CanvasGeometry -> DesktopCoordinate -> ScreenCoordinate
desktop2Screen geometry = canvas2Screen geometry . desktop2Canvas geometry

-- device2pageCoord :: CanvasGeometry -> PointerCoord -> Page


core2Desktop :: CanvasGeometry -> (Double,Double) -> DesktopCoordinate 
core2Desktop geometry = screen2Desktop geometry . ScrCoord 

wacom2Desktop :: CanvasGeometry -> (Double,Double) -> DesktopCoordinate
wacom2Desktop geometry (x,y) = let Dim w h = unScreenDimension (screenDim geometry)
                               in screen2Desktop geometry . ScrCoord $ (w*x,h*y) 
                                  
device2Desktop :: CanvasGeometry -> PointerCoord -> DesktopCoordinate 
device2Desktop geometry (PointerCoord typ x y) =  
  case typ of 
    Core -> core2Desktop geometry (x,y)
    Stylus -> wacom2Desktop geometry (x,y)
    Eraser -> wacom2Desktop geometry (x,y)
device2Desktop geometry NoPointerCoord = error "NoPointerCoordinate device2Desktop"
         