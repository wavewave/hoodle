module Application.HXournal.Draw where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Data.IORef

import Text.Xournal.Type
import Text.Xournal.Parse
import Graphics.Xournal.Render 

import Application.HXournal.Device
import Control.Applicative 


data CanvasPageGeometry = 
  CanvasPageGeometry { screen_size :: (Double,Double) 
                     , canvas_size :: (Double,Double)
                     , page_size :: (Double,Double)
                     , canvas_origin :: (Double,Double) 
                     }


{-
canvasCoord2xournalCoord :: (Double,Double) -> (Double,Double) 
                            -> (Double,Double) 
                            -> IO (Double,Double)
canvasCoord2xournalCoord (w',h') (w,h) (x,y) = do 
  return (x * w / realToFrac w', y * h / realToFrac h')
-}
  
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

core2pageCoord :: CanvasPageGeometry -> (Double,Double) -> (Double,Double)
core2pageCoord (CanvasPageGeometry _ (w',h') (w,h) _) (px,py) = 
  (px * w / w', py * h / h')
  
wacom2pageCoord :: CanvasPageGeometry 
                   -> (Double,Double) 
                   -> (Double,Double)
wacom2pageCoord (CanvasPageGeometry (ws,hs) (w',h') (w,h) (x0,y0)) (px,py) 
  = let (x1,y1) = (ws*px-x0,hs*py-y0)
    in  (x1 * w / w', y1 * h / h')

device2pageCoord :: CanvasPageGeometry 
                 -> PointerCoord  
                 -> (Double,Double)
device2pageCoord cpg pcoord = 
 let (px,py) = (,) <$> pointerX <*> pointerY $ pcoord  
 in case pointerType pcoord of 
      Core -> core2pageCoord  cpg (px,py)
      _    -> wacom2pageCoord cpg (px,py)



updateCanvas :: DrawingArea -> Xournal -> Int -> IO ()
updateCanvas canvas xoj pagenum = do 
  let totalnumofpages = (length . xoj_pages) xoj
  let currpage = ((!!pagenum).xoj_pages) xoj
  win <- widgetGetDrawWindow canvas
  (w',h') <- widgetGetSize canvas
  let (Dim w h) = page_dim currpage
  renderWithDrawable win $ do
    scale (realToFrac w' / w) (realToFrac h' / h)
    cairoDrawPage currpage
  return ()

drawSegment :: CanvasPageGeometry 
               -> Double 
               -> (Double,Double,Double,Double) 
               -> (Double,Double) 
               -> (Double,Double) 
               -> Render () 
drawSegment cpg wdth (r,g,b,a) (x0,y0) (x,y) = do 
  let (w,h) = page_size cpg 
      (w',h') = canvas_size cpg 
  setLineWidth wdth
  scale (w'/w) (h'/h)
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
  