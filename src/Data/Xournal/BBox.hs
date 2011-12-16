module Data.Xournal.BBox where

import Data.ByteString hiding (map,maximum,minimum)
import Data.Xournal.Generic
import Data.Xournal.Simple
import Data.Strict.Tuple 
import Prelude hiding (fst,snd)

data BBox = BBox { bbox_upperleft :: (Double,Double) 
                 , bbox_lowerright :: (Double,Double) } 
          deriving (Show)

data StrokeBBox = StrokeBBox { strokebbox_tool :: ByteString
                             , strokebbox_color :: ByteString 
                             , strokebbox_width :: Double
                             , strokebbox_data :: [Pair Double Double] 
                             , strokebbox_bbox :: BBox }
                deriving (Show)

type TLayerBBox = GLayer [] StrokeBBox 

type TPageBBox = GPage Background [] TLayerBBox 

type TXournalBBox = GXournal [] TPageBBox

instance GStrokeable StrokeBBox where  
  gFromStroke = mkStrokeBBoxFromStroke 
  gToStroke = strokeFromStrokeBBox

mkbbox :: [Pair Double Double] -> BBox 
mkbbox lst = let xs = map fst lst 
                 ys = map snd lst
             in  BBox { bbox_upperleft = (minimum xs, minimum ys)
                      , bbox_lowerright = (maximum xs, maximum ys) } 


mkStrokeBBoxFromStroke :: Stroke -> StrokeBBox
mkStrokeBBoxFromStroke str = 
  StrokeBBox { strokebbox_tool = stroke_tool str 
             , strokebbox_color = stroke_color str 
             , strokebbox_width = stroke_width str 
             , strokebbox_data = stroke_data str 
             , strokebbox_bbox = mkbbox (stroke_data str) } 

strokeFromStrokeBBox :: StrokeBBox -> Stroke 
strokeFromStrokeBBox strbbox = 
  Stroke { stroke_tool = strokebbox_tool strbbox
         , stroke_color = strokebbox_color strbbox
         , stroke_width= strokebbox_width strbbox
         , stroke_data = strokebbox_data strbbox } 


{-
data XournalBBox = XournalBBox { xojbbox_pages :: [PageBBox] }

data PageBBox = PageBBox { pagebbox_dim :: Dimension
                         , pagebbox_bkg :: Background
                         , pagebbox_layers :: [LayerBBox] } 

data LayerBBox = LayerBBox { layerbbox_strokes :: [StrokeBBox] } 
-}

