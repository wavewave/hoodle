{-# LANGUAGE OverloadedStrings #-}

module Graphics.Xournal.Render where 

import Graphics.Rendering.Cairo

import Data.Strict.Tuple

import Text.Xournal.Type 
import Text.Xournal.Predefined 

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S

drawOneStroke :: Stroke -> Render ()
drawOneStroke s = do 
  case M.lookup (stroke_color s) predefined_pencolor of
    Just (r,g,b,a) -> setSourceRGBA r g b a 
    Nothing -> setSourceRGBA 0 0 0 1
  setLineWidth (stroke_width s) 
  drawOneStrokeCurve (stroke_data s)
  stroke

drawOneStrokeCurve :: [Pair Double Double] -> Render ()
drawOneStrokeCurve ((x0 :!: y0) : xs) = do 
  x0 `seq` y0 `seq` moveTo x0 y0
  mapM_ f xs 
    where f (x :!: y) = x `seq` y `seq` lineTo x y 

cairoDrawBackground :: Page -> Render () 
cairoDrawBackground page = do 
  let Background typ col sty = page_bkg page
      Dim w h = page_dim page  
  let c = M.lookup col predefined_bkgcolor  
  case c of 
    Just (r,g,b,a) -> setSourceRGB r g b 
    Nothing        -> setSourceRGB 1 1 1 
  rectangle 0 0 w h 
  fill
  cairoDrawRuling w h sty

cairoDrawRuling :: Double -> Double -> S.ByteString -> Render () 
cairoDrawRuling w h style = do
  let drawHorizRules = do 
      let (r,g,b,a) = predefined_RULING_COLOR         
      setSourceRGBA r g b a 
      setLineWidth predefined_RULING_THICKNESS
      let drawonerule y = do 
            moveTo 0 y 
            lineTo w y
            stroke  
      mapM_ drawonerule [predefined_RULING_TOPMARGIN
                        ,predefined_RULING_TOPMARGIN+predefined_RULING_SPACING
                        ..
                        h-1]
  case style of 
    "plain" -> return () 
    "lined" -> do 
      drawHorizRules
      let (r2,g2,b2,a2) = predefined_RULING_MARGIN_COLOR
      setSourceRGBA r2 g2 b2 a2 
      setLineWidth predefined_RULING_THICKNESS
      moveTo predefined_RULING_LEFTMARGIN 0 
      lineTo predefined_RULING_LEFTMARGIN h
      stroke
    "ruled" -> drawHorizRules 
    "graph" -> do 
      let (r3,g3,b3,a3) = predefined_RULING_COLOR 
      setSourceRGBA r3 g3 b3 a3 
      setLineWidth predefined_RULING_THICKNESS
      let drawonegraphvert x = do 
            moveTo x 0 
            lineTo x h
            stroke  
      let drawonegraphhoriz y = do 
            moveTo 0 y
            lineTo w y
            stroke
      mapM_ drawonegraphvert  [0,predefined_RULING_GRAPHSPACING..w-1] 
      mapM_ drawonegraphhoriz [0,predefined_RULING_GRAPHSPACING..h-1]
    _ -> return ()     

cairoDrawPage :: Page -> Render ()
cairoDrawPage page = do 
  let strokes = (layer_strokes . (!!0) . page_layers ) page 
      (Dim w h) = page_dim page
  cairoDrawBackground page
  setSourceRGB 0 0 0
  setLineWidth 1
  setLineCap LineCapRound
  setLineJoin LineJoinRound

  mapM_ drawOneStroke strokes
  stroke


