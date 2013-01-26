{-# LANGUAGE TypeFamilies, TypeOperators, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Type.Item 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Renderable Item Type  
-- 
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Type.Item where

import Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.SVG as RSVG
-- from hoodle-platform 
import Data.Hoodle.BBox 
import Data.Hoodle.Simple

data RItem = RItemStroke (BBoxed Stroke)
           | RItemImage (BBoxed Image) (Maybe Surface)
           | RItemSVG (BBoxed SVG) (Maybe RSVG.SVG)

instance GetBBoxable RItem where
  getBBox (RItemStroke strk) = getBBox strk
  getBBox (RItemImage img _) = getBBox img 
  getBBox (RItemSVG svg _) = getBBox svg 

instance Show RItem where
  show (RItemStroke strk) = "RItemStroke " ++ show strk
  show (RItemImage img _) = "RItemImage " ++ show img 
  show (RItemSVG svg _) = "RItemSVG " ++ show svg

-- | 
isStrkInRItem :: RItem -> Bool 
isStrkInRItem (RItemStroke _) = True
isStrkInRItem _ = False

-- |
isImgInRItem :: RItem -> Bool 
isImgInRItem (RItemImage _ _) = True
isImgInRItem _ = False

-- |
isSVGInRItem :: RItem -> Bool 
isSVGInRItem (RItemSVG _ _) = True
isSVGInRItem _ = False


-- | 
findStrkInRItem :: RItem -> Maybe (BBoxed Stroke)
findStrkInRItem (RItemStroke strk) = Just strk
findStrkInRItem _ = Nothing 

-- | 
findImgInRItem :: RItem -> Maybe (BBoxed Image)
findImgInRItem (RItemImage img _ ) = Just img 
findImgInRItem _ = Nothing 


-- | 
findSVGInRItem :: RItem -> Maybe (BBoxed SVG)
findSVGInRItem (RItemSVG svg _) = Just svg 
findSVGInRItem _ = Nothing 



-- |
rItem2Item :: RItem -> Item 
rItem2Item (RItemStroke strk) = (ItemStroke . bbxed_content) strk
rItem2Item (RItemImage img _) = (ItemImage . bbxed_content) img
rItem2Item (RItemSVG svg _) = (ItemSVG. bbxed_content) svg

{-
-- | 
rItemBBox :: RItem -> BBox 
rItemBBox (RItemStroke strk) = strkbbx_bbx strk
rItemBBox (RItemImage img _) = imgbbx_bbx img

-}
