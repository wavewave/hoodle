{-# LANGUAGE TypeFamilies #-}

module Data.Xournal.Map where

import Data.IntMap 
import Data.Xournal.Simple
import Data.Xournal.Generic
import Data.Xournal.BBox

type TPageMap = GPage Background IntMap TLayerSimple 

type TXournalMap = GXournal [] TPageMap 

type TPageBBoxMap = GPage Background IntMap TLayerBBox

type TXournalBBoxMap = GXournal IntMap TPageBBoxMap

type TPageBBoxMapBkg b = GPage b IntMap TLayerBBox

type TXournalBBoxMapBkg b = GXournal IntMap (TPageBBoxMapBkg b)


{-
data XournalBBoxMap = XournalBBoxMap { xbm_pages :: M.IntMap PageBBoxMap } 

data PageBBoxMap = PageBBoxMap { pbm_dim :: Dimension
                               , pbm_bkg :: Background
                               , pbm_layers :: M.IntMap LayerBBox }
-}

{-
instance IPage PageBBoxMap where
  type TLayer PageBBoxMap = LayerBBox
  pageDim = pbm_dim 
  pageBkg = pbm_bkg
  pageLayers = M.elems . pbm_layers
  
instance IXournal XournalBBoxMap where
  type TPage XournalBBoxMap = PageBBoxMap 
  xournalPages = M.elems . xbm_pages 
  
mkXournalBBoxMapFromXournalBBox :: XournalBBox -> XournalBBoxMap 
mkXournalBBoxMapFromXournalBBox xoj = 
  XournalBBoxMap { xbm_pages = M.map mkPageBBoxMapFromPageBBox 
                             . M.fromList 
                             $ zip [0..] (xojbbox_pages xoj) }  

mkPageBBoxMapFromPageBBox :: PageBBox -> PageBBoxMap
mkPageBBoxMapFromPageBBox page = 
  PageBBoxMap { pbm_dim = pagebbox_dim page
              , pbm_bkg = pagebbox_bkg page
              , pbm_layers = M.fromList $ zip [0..] (pagebbox_layers page) }

mkXournalBBoxMapFromXournal :: Xournal -> XournalBBoxMap 
mkXournalBBoxMapFromXournal = mkXournalBBoxMapFromXournalBBox . mkXournalBBoxFromXournal 

xournalFromXournalBBoxMap :: XournalBBoxMap -> Xournal 
xournalFromXournalBBoxMap xoj = 
  emptyXournal { xoj_pages = map pageFromPageBBoxMap (xournalPages xoj) } 
  
pageFromPageBBoxMap :: PageBBoxMap -> Page 
pageFromPageBBoxMap page = 
  let pbox  = PageBBox { pagebbox_dim = pbm_dim page
                       , pagebbox_bkg = pbm_bkg page
                       , pagebbox_layers = pageLayers page }
  in pageFromPageBBox pbox 
 
-}  
  
  