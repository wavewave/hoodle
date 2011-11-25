module Application.HXournal.Type.XournalBBox where

import Text.Xournal.Type 
import Data.Strict.Tuple
import Data.ByteString hiding (map, minimum, maximum)


import Prelude hiding (fst,snd,curry,uncurry)

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
                         
data BBox = BBox { bbox_upperleft :: (Double,Double) 
                 , bbox_lowerright :: (Double,Double) } 
          deriving (Show)
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