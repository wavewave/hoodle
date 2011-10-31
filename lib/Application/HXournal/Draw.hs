module Application.HXournal.Draw where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Data.IORef

import Text.Xournal.Type
import Text.Xournal.Parse
import Graphics.Xournal.Render 

{-
refresh_xournal :: FilePath -> IO Xournl
refresh_xournal xojref str = do 
  xoj <- read_xojgz str 
  writeIORef xojref $! xoj
-}

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

