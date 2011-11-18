module Application.HXournal.Draw where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Data.IORef

import Text.Xournal.Type
import Text.Xournal.Parse
import Graphics.Xournal.Render 

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
  