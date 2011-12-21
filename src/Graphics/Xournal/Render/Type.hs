{-# LANGUAGE TypeFamilies #-}

module Graphics.Xournal.Render.Type where

import Prelude hiding (fst,snd)
import Data.Strict.Tuple 
import Data.ByteString hiding (map, minimum, maximum)

import Data.Xournal.Simple 
import Data.Xournal.Generic
import Data.Xournal.BBox
import Data.Xournal.Map

import Data.IntMap hiding (map)

data AlterList a b = Empty | a :- AlterList b a
                   deriving (Show)

infixr 6 :-

newtype NotHitted a = NotHitted { unNotHitted :: [a] } 
                    deriving (Show)

newtype Hitted a = Hitted { unHitted :: [a] } 
                   deriving (Show)

type StrokeHitted = AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox) 


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


type TAlterHitted a = AlterList [a] (Hitted a)

newtype TEitherAlterHitted a = 
          TEitherAlterHitted { 
            unTEitherAlterHitted :: Either [a] (TAlterHitted a)
          }

type TLayerSelect a = GLayer TEitherAlterHitted (StrokeTypeFromLayer a) 

type TLayerSelectBuf a = GLayerBuf (BufTypeFromLayer a) TEitherAlterHitted (StrokeTypeFromLayer a) 

type family StrokeTypeFromLayer a  :: * 
     
type family BufTypeFromLayer a :: *
     
type instance BufTypeFromLayer (GLayerBuf b s a) = b     
     
type instance StrokeTypeFromLayer TLayerBBox = StrokeBBox

data TLayerSelectInPage s a = TLayerSelectInPage 
                              { gselectedlayer :: TLayerSelect a 
                              , gotherlayers :: s a
                              }

data TLayerSelectInPageBuf s a = TLayerSelectInPageBuf
                               { gselectedlayerbuf :: TLayerSelectBuf a 
                               , gotherlayersbuf :: s a
                               }


type TTempPageSelect = GPage Background (TLayerSelectInPage []) TLayerBBox
                       
type TTempXournalSelect = GSelect (IntMap TPageBBoxMap) (Maybe (Int, TTempPageSelect))


{-                         

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
  
-}
 
{-
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
  
-}  
----



