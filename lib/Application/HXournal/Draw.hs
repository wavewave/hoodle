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
                     }
  
getCanvasPageGeometry :: DrawingArea -> Page -> IO CanvasPageGeometry
getCanvasPageGeometry canvas page = do 
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

core2pageCoord :: CanvasPageGeometry -> ZoomMode 
                  -> (Double,Double) -> (Double,Double)
core2pageCoord (CanvasPageGeometry _ (w',h') (w,h) _) zmode (px,py) = 
  let s = case zmode of 
            Original -> 1.0 
            FitWidth -> (w/w')
  in (px * s, py * s)
  
wacom2pageCoord :: CanvasPageGeometry 
                   -> ZoomMode 
                   -> (Double,Double) 
                   -> (Double,Double)
wacom2pageCoord (CanvasPageGeometry (ws,hs) (w',h') (w,h) (x0,y0)) 
                zmode 
                (px,py) 
  = let (x1,y1) = (ws*px-x0,hs*py-y0)
        s = case zmode of
              Original -> 1.0
              FitWidth -> w/w'
    in  (x1*s, y1*s)

device2pageCoord :: CanvasPageGeometry 
                 -> ZoomMode 
                 -> PointerCoord  
                 -> (Double,Double)
device2pageCoord cpg zmode pcoord = 
 let (px,py) = (,) <$> pointerX <*> pointerY $ pcoord  
 in case pointerType pcoord of 
      Core -> core2pageCoord  cpg zmode (px,py)
      _    -> wacom2pageCoord cpg zmode (px,py)



updateCanvas :: DrawingArea -> Xournal -> Int -> ZoomMode -> IO ()
updateCanvas canvas xoj pagenum zmode = do 
  let totalnumofpages = (length . xoj_pages) xoj
  let currpage = ((!!pagenum).xoj_pages) xoj
  win <- widgetGetDrawWindow canvas
  (w',h') <- widgetGetSize canvas
  let (Dim w h) = page_dim currpage
  renderWithDrawable win $ do
    case zmode of 
      FitWidth -> do 
        let s = realToFrac w' / w 
        scale s s 
        -- scale (realToFrac w' / w) (realToFrac h' / h)
        return ()
      Original -> return () 
      Zoom factor -> do 
        scale factor factor
        return ()
    cairoDrawPage currpage
  return ()

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
    let (w,h) = page_size cpg 
        (w',h') = canvas_size cpg 
        s = case zmode of 
              Original -> 1.0 
              FitWidth -> w'/w
    scale s s 
    setSourceRGBA r g b a
    setLineWidth wdth
    moveTo x0 y0
    lineTo x y
    stroke
  
penMoveTo :: DrawingArea -> (Double,Double) -> IO ()
penMoveTo canvas (x,y) = do
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win $ do
    setLineWidth 1.0
    setSourceRGBA 0.1 0.1 0.1 1
    moveTo x y
  return ()

penLineTo canvas (x0,y0) (x,y) = do 
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win $ do 
    setLineWidth 1.0
    setSourceRGBA 0.1 0.1 0.1 1
    moveTo x0 y0 
    lineTo x y
    stroke
  return ()
  