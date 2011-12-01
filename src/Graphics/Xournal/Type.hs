{-# LANGUAGE TypeFamilies #-}

module Graphics.Xournal.Type where
               
import Data.Strict.Tuple
import Data.ByteString hiding (map, minimum, maximum)

import Text.Xournal.Type 

data AlterList a b = Empty | a :- AlterList b a
                   deriving (Show)

infixr 6 :-

fmapAL :: (a -> c) -> (b -> d) -> AlterList a b -> AlterList c d
fmapAL _ _ Empty = Empty 
fmapAL f g (x :- ys) = f x :- fmapAL g f ys 


getA :: AlterList a b -> [a] 
getA Empty = [] 
getA (x :- xs)  = x : getB xs 

getB :: AlterList a b -> [b]
getB Empty = [] 
getB (x :- xs) = getA xs 

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

