{-# LANGUAGE OverloadedStrings #-}

module Graphics.Xournal.Render.BBox where

import Graphics.Rendering.Cairo
import Graphics.Xournal.Render
import Graphics.Xournal.HitTest
import Graphics.Xournal.Type 

import Text.Xournal.Type 
import Text.Xournal.Predefined 

import qualified Data.Map as M

import Data.Strict.Tuple
import Data.ByteString hiding (map, minimum, maximum, concat, concatMap, filter )


import Prelude hiding (fst,snd,curry,uncurry)

  
mkXournalBBoxFromXournal :: Xournal -> XournalBBox 
mkXournalBBoxFromXournal xoj = 
  XournalBBox { xojbbox_pages = map mkPageBBoxFromPage (xoj_pages xoj) } 
  
mkPageBBoxFromPage :: Page -> PageBBox
mkPageBBoxFromPage pg = 
  PageBBox { pagebbox_dim = page_dim pg 
           , pagebbox_bkg = page_bkg pg 
           , pagebbox_layers = map mkLayerBBoxFromLayer (page_layers pg) }
  
mkLayerBBoxFromLayer :: Layer -> LayerBBox 
mkLayerBBoxFromLayer ly = 
  LayerBBox { layerbbox_strokes = map mkStrokeBBoxFromStroke (layer_strokes ly) } 
  
mkStrokeBBoxFromStroke :: Stroke -> StrokeBBox
mkStrokeBBoxFromStroke str = 
  StrokeBBox { strokebbox_tool = stroke_tool str 
             , strokebbox_color = stroke_color str 
             , strokebbox_width = stroke_width str 
             , strokebbox_data = stroke_data str 
             , strokebbox_bbox = mkbbox (stroke_data str) } 
  
mkbbox :: [Pair Double Double] -> BBox 
mkbbox lst = let xs = map fst lst 
                 ys = map snd lst
             in  BBox { bbox_upperleft = (minimum xs, minimum ys)
                      , bbox_lowerright = (maximum xs, maximum ys) } 
 
xournalFromXournalBBox :: XournalBBox -> Xournal 
xournalFromXournalBBox xojbbox = 
  emptyXournal { xoj_pages = map pageFromPageBBox (xojbbox_pages xojbbox) }

pageFromPageBBox :: PageBBox -> Page 
pageFromPageBBox pgbbox = 
  Page { page_dim = pagebbox_dim pgbbox 
       , page_bkg = pagebbox_bkg pgbbox
       , page_layers = map layerFromLayerBBox (pagebbox_layers pgbbox) } 
  
layerFromLayerBBox :: LayerBBox -> Layer 
layerFromLayerBBox lybbox = 
  Layer { layer_strokes = map strokeFromStrokeBBox (layerbbox_strokes lybbox) }
  
strokeFromStrokeBBox :: StrokeBBox -> Stroke 
strokeFromStrokeBBox strbbox = 
  Stroke { stroke_tool = strokebbox_tool strbbox
         , stroke_color = strokebbox_color strbbox
         , stroke_width= strokebbox_width strbbox
         , stroke_data = strokebbox_data strbbox } 
  
----

emptyLayer :: Layer 
emptyLayer = Layer { layer_strokes = [] }

newPageFromOld :: Page -> Page
newPageFromOld page = 
  Page { page_dim = page_dim page 
       , page_bkg = page_bkg page 
       , page_layers = [emptyLayer] } 
                   
----

cairoOneStrokeBBoxOnly :: StrokeBBox -> Render () 
cairoOneStrokeBBoxOnly s = do  
  case M.lookup (strokeColor s) predefined_pencolor of 
    Just (r,g,b,a) -> setSourceRGBA r g b a
    Nothing -> setSourceRGBA 0 0 0 1 
  setLineWidth (strokeWidth s) 
  let BBox (x1,y1) (x2,y2) = strokebbox_bbox s
  rectangle x1 y1 (x2-x1) (y2-y1)
  stroke
  
cairoDrawPageBBoxOnly :: PageBBox -> Render ()  
cairoDrawPageBBoxOnly page = do
    let strokes = (layerStrokes . (!!0) . pageLayers) page
        (Dim w h) = pageDim page
    cairoDrawBackground page 
    mapM_ cairoOneStrokeBBoxOnly strokes
    

----

inflate :: BBox -> Double -> BBox 
inflate (BBox (x1,y1) (x2,y2)) r = BBox (x1-r,y1-r) (x2+r,y2+r)

----

cairoDrawPageBBox :: Maybe BBox -> PageBBox -> Render ()
cairoDrawPageBBox mbbox page = do 
  cairoDrawBackgroundBBox mbbox (pagebbox_dim page) (pagebbox_bkg page) 
  mapM_ (cairoDrawLayerBBox mbbox) (pagebbox_layers page)


cairoDrawLayerBBox :: Maybe BBox -> LayerBBox -> Render () 
cairoDrawLayerBBox mbbox layer = do  
  let hittestbbox = case mbbox of 
                       Nothing -> NotHitted [] 
                                  :- Hitted (layerbbox_strokes layer) 
                                  :- Empty 
                       Just bbox -> mkHitTestBBoxBBox bbox (layerbbox_strokes layer)
  mapM_ drawOneStroke . concatMap unHitted  . getB $ hittestbbox


cairoDrawBackgroundBBox :: Maybe BBox -> Dimension -> Background -> Render ()
cairoDrawBackgroundBBox mbbox (Dim w h) (Background typ col sty) = 
    case mbbox of 
      Nothing -> cairoDrawBkg (Dim w h) (Background typ col sty)
      Just bbox@(BBox (x1,y1) (x2,y2)) -> do 
        let c = M.lookup col predefined_bkgcolor  
        case c of 
          Just (r,g,b,a) -> setSourceRGB r g b 
          Nothing        -> setSourceRGB 1 1 1 
        rectangle x1 y1 (x2-x1) (y2-y1)
        fill
        cairoDrawRulingBBox bbox w h sty

cairoDrawRulingBBox :: BBox -> Double -> Double -> ByteString -> Render () 
cairoDrawRulingBBox bbox@(BBox (x1,y1) (x2,y2)) w h style = do
  let drawonerule y = do 
        moveTo x1 y 
        lineTo x2 y
        stroke  
  let drawonegraphvert x = do 
        moveTo x y1 
        lineTo x y2
        stroke  
  let drawonegraphhoriz y = do 
        moveTo x1 y
        lineTo x2 y
        stroke
      fullRuleYs = [ predefined_RULING_TOPMARGIN 
                   , predefined_RULING_TOPMARGIN+predefined_RULING_SPACING
                   .. 
                   h-1 ]
      ruleYs = filter (\y-> (y <= y2) && (y >= y1)) fullRuleYs
      fullGraphXs = [0,predefined_RULING_GRAPHSPACING..w-1]          
      fullGraphYs = [0,predefined_RULING_GRAPHSPACING..h-1]
      graphXs = filter (\x->(x<=x2)&&(x>=x1)) fullGraphXs
      graphYs = filter (\y->(y<=y2)&&(y>=y1)) fullGraphYs 
  
  let drawHorizRules = do 
      let (r,g,b,a) = predefined_RULING_COLOR         
      setSourceRGBA r g b a 
      setLineWidth predefined_RULING_THICKNESS
      mapM_ drawonerule ruleYs
  
  
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
      mapM_ drawonegraphvert  graphXs 
      mapM_ drawonegraphhoriz graphYs
    _ -> return ()     
