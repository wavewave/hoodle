module Application.HXournal.Draw where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Data.IORef

import Text.Xournal.Type
import Text.Xournal.Parse
import Graphics.Xournal.Render 

import Application.HXournal.Type 
import Application.HXournal.Device
import Control.Applicative 


data CanvasPageGeometry = 
  CanvasPageGeometry { screen_size :: (Double,Double) 
                     , canvas_size :: (Double,Double)
                     , page_size :: (Double,Double)
                     , canvas_origin :: (Double,Double) 
                     , page_origin :: (Double,Double)
                     }
  deriving (Show)  

getCanvasPageGeometry :: DrawingArea -> Page -> (Double,Double) 
                         -> IO CanvasPageGeometry
getCanvasPageGeometry canvas page (xorig,yorig) = do 
  win <- widgetGetDrawWindow canvas
  (w',h') <- widgetGetSize canvas
  screen <- widgetGetScreen canvas
  (ws,hs) <- (,) <$> screenGetWidth screen <*> screenGetHeight screen
  let (Dim w h) = page_dim page
  (x0,y0) <- drawWindowGetOrigin win
  return $ CanvasPageGeometry (fromIntegral ws, fromIntegral hs) 
                              (fromIntegral w', fromIntegral h') 
                              (w,h) 
                              (fromIntegral x0,fromIntegral y0)
                              (xorig, yorig)

core2pageCoord :: CanvasPageGeometry -> ZoomMode 
                  -> (Double,Double) -> (Double,Double)
core2pageCoord cpg zmode (px,py) = 
  let s =  1.0 / getRatioFromPageToCanvas cpg zmode 
  in (px * s, py * s)
  
wacom2pageCoord :: CanvasPageGeometry 
                   -> ZoomMode 
                   -> (Double,Double) 
                   -> (Double,Double)
wacom2pageCoord cpg@(CanvasPageGeometry (ws,hs) (w',h') (w,h) (x0,y0) (xorig,yorig)) 
                zmode 
                (px,py) 
  = let (x1,y1) = (ws*px-x0,hs*py-y0)
        s = 1.0 / getRatioFromPageToCanvas cpg zmode
        (xo,yo) = case zmode of
                    Original -> (xorig,yorig)
                    FitWidth -> (0,yorig)
    in  (x1*s+xo,y1*s+yo)

device2pageCoord :: CanvasPageGeometry 
                 -> ZoomMode 
                 -> PointerCoord  
                 -> (Double,Double)
device2pageCoord cpg zmode pcoord = 
 let (px,py) = (,) <$> pointerX <*> pointerY $ pcoord  
 in case pointerType pcoord of 
      Core -> core2pageCoord  cpg zmode (px,py)
      _    -> wacom2pageCoord cpg zmode (px,py)

transformForPageCoord :: CanvasPageGeometry -> ZoomMode -> Render ()
transformForPageCoord cpg zmode = do 
  let (xo,yo) = page_origin cpg
  let s = getRatioFromPageToCanvas cpg zmode  
  scale s s
  translate (-xo) (-yo)      
  
updateCanvas :: DrawingArea -> Xournal -> Int -> ViewMode -> IO ()
updateCanvas canvas xoj pagenum vmode = do 
  let zmode = vm_zmmode vmode
      origin = vm_viewportOrigin vmode
  let totalnumofpages = (length . xoj_pages) xoj
  let currpage = ((!!pagenum).xoj_pages) xoj
  geometry <- getCanvasPageGeometry canvas currpage origin
  win <- widgetGetDrawWindow canvas
  (w',h') <- widgetGetSize canvas
  let (Dim w h) = page_dim currpage
  renderWithDrawable win $ do
    transformForPageCoord geometry zmode
    cairoDrawPage currpage
  return ()

getRatioFromPageToCanvas :: CanvasPageGeometry -> ZoomMode -> Double 
getRatioFromPageToCanvas cpg Original = 1.0 
getRatioFromPageToCanvas cpg FitWidth = 
  let (w,_)  = page_size cpg 
      (w',_) = canvas_size cpg 
  in  w'/w
getRatioFromPageToCanvas cpg (Zoom s) = s 

drawSegment :: DrawingArea
               -> CanvasPageGeometry 
               -> ZoomMode 
               -> Double 
               -> (Double,Double,Double,Double) 
               -> (Double,Double) 
               -> (Double,Double) 
               -> IO () 
drawSegment canvas cpg zmode wdth (r,g,b,a) (x0,y0) (x,y) = do 
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win $ do
    transformForPageCoord cpg zmode
    setSourceRGBA r g b a
    setLineWidth wdth
    moveTo x0 y0
    lineTo x y
    stroke
  
{-
penMoveTo :: DrawingArea -> (Double,Double) -> IO ()
penMoveTo canvas (x,y) = do
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win $ do
    setLineWidth 1.0
    setSourceRGBA 0.1 0.1 0.1 1
    moveTo x y
  return ()
-}

{-
penLineTo canvas (x0,y0) (x,y) = do 
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win $ do 
    setLineWidth 1.0
    setSourceRGBA 0.1 0.1 0.1 1
    moveTo x0 y0 
    lineTo x y
    stroke
  return ()
-}