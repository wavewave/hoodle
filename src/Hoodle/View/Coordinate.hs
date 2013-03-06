{-# LANGUAGE GADTs, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.View.Coordinate
-- Copyright   : (c) 2012,2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.View.Coordinate where 


import           Control.Applicative
import           Control.Lens (view,set,over)
import           Control.Monad 
import           Data.Foldable (toList)
import qualified Data.IntMap as M
import           Data.Maybe
import           Data.Monoid
import           Graphics.UI.Gtk hiding (get,set)
-- from hoodle-platform
import Data.Hoodle.Simple (Dimension(..))
import Data.Hoodle.Generic
import Data.Hoodle.BBox 
-- from this package
import Hoodle.Device
import Hoodle.Type.Canvas
import Hoodle.Type.PageArrangement
import Hoodle.Type.Alias
-- 

-- | data structure for transformation among screen, canvas, desktop and page coordinates

data CanvasGeometry = 
  CanvasGeometry 
  { screenDim :: ScreenDimension
  , canvasDim :: CanvasDimension
  , desktopDim :: DesktopDimension 
  , canvasViewPort :: ViewPortBBox -- ^ in desktop coordinate 
  , screen2Canvas :: ScreenCoordinate -> CanvasCoordinate
  , canvas2Screen :: CanvasCoordinate -> ScreenCoordinate
  , canvas2Desktop :: CanvasCoordinate -> DesktopCoordinate
  , desktop2Canvas :: DesktopCoordinate -> CanvasCoordinate
  , desktop2Page :: DesktopCoordinate -> Maybe (PageNum,PageCoordinate)
  , page2Desktop :: (PageNum,PageCoordinate) -> DesktopCoordinate
  } 

-- | make a canvas geometry data structure from current status 
makeCanvasGeometry :: PageNum 
                      -> PageArrangement vm 
                      -> DrawingArea 
                      -> IO CanvasGeometry 
makeCanvasGeometry cpn arr canvas = do 
  win <- widgetGetDrawWindow canvas
  let cdim@(CanvasDimension (Dim w' h')) = view canvasDimension arr
  screen <- widgetGetScreen canvas
  (ws,hs) <- (,) <$> (fromIntegral <$> screenGetWidth screen)
                 <*> (fromIntegral <$> screenGetHeight screen)
  (x0,y0) <- return . ((,) <$> fromIntegral.fst <*> fromIntegral.snd ) =<< drawWindowGetOrigin win
  let corig = CanvasOrigin (x0,y0)
      (deskdim, cvsvbbox, p2d, d2p) = 
        case arr of  
          SingleArrangement _ pdim vbbox -> ( DesktopDimension . unPageDimension $ pdim
                                               , vbbox
                                               , DeskCoord . unPageCoord . snd
                                               , \(DeskCoord coord) ->Just (cpn,(PageCoord coord)) )
          ContinuousArrangement _ ddim pfunc vbbox -> 
            ( ddim, vbbox, makePage2Desktop pfunc, makeDesktop2Page pfunc ) 
  let s2c = xformScreen2Canvas corig
      c2s = xformCanvas2Screen corig
      c2d = xformCanvas2Desk cdim cvsvbbox 
      d2c = xformDesk2Canvas cdim cvsvbbox
  return $ CanvasGeometry (ScreenDimension (Dim ws hs)) (CanvasDimension (Dim w' h')) 
                          deskdim cvsvbbox s2c c2s c2d d2c d2p p2d
    
-- |
makePage2Desktop :: (PageNum -> Maybe (PageOrigin,PageDimension)) 
                 -> (PageNum,PageCoordinate) 
                 -> DesktopCoordinate
makePage2Desktop pfunc (pnum,PageCoord (x,y)) = 
  maybe (DeskCoord (-100,-100)) -- temporary 
        (\(PageOrigin (x0,y0),_) -> DeskCoord (x0+x,y0+y)) 
        (pfunc pnum) 
     
-- | 
makeDesktop2Page :: (PageNum -> Maybe (PageOrigin,PageDimension)) 
                 -> DesktopCoordinate 
                 -> Maybe (PageNum, PageCoordinate)
makeDesktop2Page pfunc (DeskCoord (x,y)) =
    if null matched 
      then Nothing 
      else let (pagenum,(PageOrigin (x0,y0),_)) = head matched  
           in Just (pagenum, PageCoord (x-x0,y-y0))
  where condition (_,(PageOrigin (x0,y0),PageDimension (Dim w h))) = 
          x >= x0 && x < x0+w && y >= y0 && y < y0+h 
        matched = filter condition 
                  . catMaybes
                  . takeWhile isJust 
                  . map ((\x'-> liftM (x',) (pfunc x')) . PageNum)
                  $ [0..] 
    


-- |   
xformScreen2Canvas :: CanvasOrigin -> ScreenCoordinate -> CanvasCoordinate
xformScreen2Canvas (CanvasOrigin (x0,y0)) (ScrCoord (sx,sy)) = CvsCoord (sx-x0,sy-y0)

-- |

xformCanvas2Screen :: CanvasOrigin -> CanvasCoordinate -> ScreenCoordinate 
xformCanvas2Screen (CanvasOrigin (x0,y0)) (CvsCoord (cx,cy)) = ScrCoord (cx+x0,cy+y0)

-- |

xformCanvas2Desk :: CanvasDimension -> ViewPortBBox -> CanvasCoordinate 
                    -> DesktopCoordinate 
xformCanvas2Desk (CanvasDimension (Dim w h)) (ViewPortBBox (BBox (x1,y1) (x2,y2))) 
                 (CvsCoord (cx,cy)) = DeskCoord (cx*(x2-x1)/w+x1,cy*(y2-y1)/h+y1) 

-- |

xformDesk2Canvas :: CanvasDimension -> ViewPortBBox -> DesktopCoordinate 
                    -> CanvasCoordinate
xformDesk2Canvas (CanvasDimension (Dim w h)) (ViewPortBBox (BBox (x1,y1) (x2,y2)))
                 (DeskCoord (dx,dy)) = CvsCoord ((dx-x1)*w/(x2-x1),(dy-y1)*h/(y2-y1))
                                       
-- | 

screen2Desktop :: CanvasGeometry -> ScreenCoordinate -> DesktopCoordinate
screen2Desktop geometry = canvas2Desktop geometry . screen2Canvas geometry  

-- | 

desktop2Screen :: CanvasGeometry -> DesktopCoordinate -> ScreenCoordinate
desktop2Screen geometry = canvas2Screen geometry . desktop2Canvas geometry

-- |

core2Desktop :: CanvasGeometry -> (Double,Double) -> DesktopCoordinate 
core2Desktop geometry = canvas2Desktop geometry . CvsCoord 

-- |

wacom2Desktop :: CanvasGeometry -> (Double,Double) -> DesktopCoordinate
wacom2Desktop geometry (x,y) = let Dim w h = unScreenDimension (screenDim geometry)
                               in screen2Desktop geometry . ScrCoord $ (w*x,h*y) 
                                  
-- |

wacom2Canvas :: CanvasGeometry -> (Double,Double) -> CanvasCoordinate                       
wacom2Canvas geometry (x,y) = let Dim w h = unScreenDimension (screenDim geometry)
                              in screen2Canvas geometry . ScrCoord $ (w*x,h*y) 
         
-- | 

device2Desktop :: CanvasGeometry -> PointerCoord -> DesktopCoordinate 
device2Desktop geometry (PointerCoord typ x y _z) =  
  case typ of 
    Core -> core2Desktop geometry (x,y)
    Stylus -> wacom2Desktop geometry (x,y)
    Eraser -> wacom2Desktop geometry (x,y)
device2Desktop _geometry NoPointerCoord = error "NoPointerCoordinate device2Desktop"
         
-- | 
getPagesInRange :: CanvasGeometry ->ViewPortBBox-> Hoodle EditMode -> [PageNum]
getPagesInRange geometry (ViewPortBBox bbox) hdl =  
  let ivbbox = Intersect (Middle bbox)
      pagemap = view gpages hdl 
      pnums = map PageNum [ 0 .. (length . toList $ pagemap)-1 ]
      pgcheck n pg = let Dim w h = view gdimension pg  
                         DeskCoord ul = page2Desktop geometry (PageNum n,PageCoord (0,0)) 
                         DeskCoord lr = page2Desktop geometry (PageNum n,PageCoord (w,h))
                         inbbox = Intersect (Middle (BBox ul lr))
                         result = ivbbox `mappend` inbbox 
                     in case result of 
                          Intersect Bottom -> False 
                          _ -> True 
      f (PageNum n) = maybe False (pgcheck n) . M.lookup n $ pagemap 
  in filter f pnums
  


-- | 
getPagesInViewPortRange :: CanvasGeometry -> Hoodle EditMode -> [PageNum]
getPagesInViewPortRange geometry hdl = 
  let vport = canvasViewPort geometry
  in getPagesInRange geometry vport hdl 

-- | 

getCvsGeomFrmCvsInfo :: (ViewMode a) => 
                        CanvasInfo a -> IO CanvasGeometry 
getCvsGeomFrmCvsInfo cinfo = do 
  let cpn = PageNum . view currentPageNum $ cinfo 
      canvas = view drawArea cinfo
      arr = view (viewInfo.pageArrangement) cinfo 
  makeCanvasGeometry cpn arr canvas 
  
-- | Get Canvas Origin in Page Coordinate : Right is successful case, 
--   Left is unsuccessful case, then return in DesktopCoordinate
  
getCvsOriginInPage :: CanvasGeometry 
                      -> Either DesktopCoordinate (PageNum, PageCoordinate) 
getCvsOriginInPage geometry = 
  let ViewPortBBox (BBox (x0,y0) (_,_)) = canvasViewPort geometry 
  in case desktop2Page geometry (DeskCoord (x0,y0)) of
       Nothing -> Left (DeskCoord (x0,y0))
       Just (pgn,pxy) -> Right (pgn,pxy)
     