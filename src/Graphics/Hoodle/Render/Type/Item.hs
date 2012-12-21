{-# LANGUAGE TypeFamilies, TypeOperators, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Type.Item 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
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

data RItem = RItemStroke StrokeBBox 
           | RItemImage ImageBBox (Maybe Surface)
           | RItemSVG SVGBBox (Maybe RSVG.SVG)

instance BBoxable RItem where
  getBBox (RItemStroke strk) = getBBox strk
  getBBox (RItemImage img _) = getBBox img 
  getBBox (RItemSVG svg _) = getBBox svg 

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
findStrkInRItem :: RItem -> Maybe StrokeBBox 
findStrkInRItem (RItemStroke strk) = Just strk
findStrkInRItem _ = Nothing 

-- | 
findImgInRItem :: RItem -> Maybe ImageBBox
findImgInRItem (RItemImage img _ ) = Just img 
findImgInRItem _ = Nothing 


-- | 
findSVGInRItem :: RItem -> Maybe SVGBBox
findSVGInRItem (RItemSVG svg _) = Just svg 
findSVGInRItem _ = Nothing 



-- |
rItem2Item :: RItem -> Item 
rItem2Item (RItemStroke strk) = (ItemStroke . strkbbx_strk) strk
rItem2Item (RItemImage img _) = (ItemImage . imgbbx_img) img
rItem2Item (RItemSVG svg _) = (ItemSVG. svgbbx_svg) svg

{-
-- | 
rItemBBox :: RItem -> BBox 
rItemBBox (RItemStroke strk) = strkbbx_bbx strk
rItemBBox (RItemImage img _) = imgbbx_bbx img

-}
