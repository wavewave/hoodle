{-# LANGUAGE TypeFamilies #-}

module Graphics.Xournal.Render.BBox where

import Graphics.Rendering.Cairo
import Graphics.Xournal.Render

import Text.Xournal.Type 
import Text.Xournal.Predefined 

import qualified Data.Map as M

import Data.Strict.Tuple
import Data.ByteString hiding (map, minimum, maximum)


import Prelude hiding (fst,snd,curry,uncurry)

data BBox = BBox { bbox_upperleft :: (Double,Double) 
                 , bbox_lowerright :: (Double,Double) } 
          deriving (Show)

data XournalBBox = XournalBBox { xojbbox_pages :: [PageBBox] }

data PageBBox = PageBBox { pagebbox_dim :: Dimension
                         , pagebbox_bkg :: Background
                         , pagebbox_layers :: [LayerBBox] } 

data LayerBBox = LayerBBox { layerbbox_strokes :: [StrokeBBox] } 

data StrokeBBox = StrokeBBox { strokebbox_tool :: ByteString
                             , strokebbox_color :: ByteString 
                             , strokebbox_width :: Double
                             , strokebbox_data :: [Pair Double Double] 
                             , strokebbox_bbox :: BBox }
                deriving (Show)
                         


instance IStroke StrokeBBox where 
  strokeTool = strokebbox_tool
  strokeColor = strokebbox_color
  strokeWidth = strokebbox_width
  strokeData = strokebbox_data
  
instance ILayer LayerBBox where 
  type TStroke LayerBBox = StrokeBBox 
  layerStrokes = layerbbox_strokes 

instance IPage PageBBox where
  type TLayer PageBBox = LayerBBox
  pageDim = pagebbox_dim
  pageBkg = pagebbox_bkg 
  pageLayers = pagebbox_layers

instance IXournal XournalBBox where
  type TPage XournalBBox = PageBBox 
  xournalPages = xojbbox_pages 
  
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

cairoDrawPageBBox :: BBox -> PageBBox -> Render ()
cairoDrawPageBBox bbox page = do 
  cairoDrawBackgroundBBox bbox (pagebbox_dim page) (pagebbox_bkg page) 
  mapM_ (cairoDrawLayerBBox bbox) (pagebbox_layers page)


cairoDrawLayerBBox :: BBox -> LayerBBox -> Render () 
cairoDrawLayerBBox (BBox (x1,y1) (x2,y2)) layer = do  
  return () 

cairoDrawBackgroundBBox :: BBox -> Dimension -> Background -> Render ()
cairoDrawBackgroundBBox (BBox (x1,y1) (x2,y2)) (Dim w h) _ = do 
  setSourceRGBA 1.0 1.0 1.0 1.0 
  rectangle x1 y1 (x2-x1) (y2-y1)
  fill
  
