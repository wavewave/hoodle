{-# LANGUAGE TypeFamilies #-}

module Graphics.Xournal.Type where

import Prelude hiding (fst,snd)
import Data.Strict.Tuple 
import Data.ByteString hiding (map, minimum, maximum)

import Text.Xournal.Type 

data AlterList a b = Empty | a :- AlterList b a
                   deriving (Show)

infixr 6 :-

newtype NotHitted = NotHitted { unNotHitted :: [StrokeBBox] } 
                  deriving (Show)

newtype Hitted = Hitted { unHitted :: [StrokeBBox] } 
                 deriving (Show)

type StrokeHitted = AlterList NotHitted Hitted 


fmapAL :: (a -> c) -> (b -> d) -> AlterList a b -> AlterList c d
fmapAL _ _ Empty = Empty 
fmapAL f g (x :- ys) = f x :- fmapAL g f ys 


getA :: AlterList a b -> [a] 
getA Empty = [] 
getA (x :- xs)  = x : getB xs 

getB :: AlterList a b -> [b]
getB Empty = [] 
getB (x :- xs) = getA xs 

interleave :: (a->c) -> (b->c) -> AlterList a b -> [c]
interleave fa fb Empty = [] 
interleave fa fb (x :- xs) = fa x : (interleave fb fa xs) 

----

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
                   


