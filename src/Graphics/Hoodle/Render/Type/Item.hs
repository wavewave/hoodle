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
-- from hoodle-platform 
import Data.Hoodle.BBox 
import Data.Hoodle.Simple

data RItem = RItemStroke StrokeBBox 
           | RItemImage ImageBBox (Maybe Surface)

instance BBoxable RItem where
  getBBox (RItemStroke strk) = getBBox strk
  getBBox (RItemImage img _) = getBBox img 

-- | 
isStrkInRItem :: RItem -> Bool 
isStrkInRItem (RItemStroke _) = True
isStrkInRItem _ = False

-- |
isImgInRItem :: RItem -> Bool 
isImgInRItem (RItemImage _ _) = True
isImgInRItem _ = False

-- | 
findStrkInRItem :: RItem -> Maybe StrokeBBox 
findStrkInRItem (RItemStroke strk) = Just strk
findStrkInRItem _ = Nothing 

-- | 
findImgInRItem :: RItem -> Maybe ImageBBox
findImgInRItem (RItemImage img _ ) = Just img 
findImgInRItem _ = Nothing 



-- |
rItem2Item :: RItem -> Item 
rItem2Item (RItemStroke strk) = (ItemStroke . strkbbx_strk) strk
rItem2Item (RItemImage img _) = (ItemImage . imgbbx_img) img

{-
-- | 
rItemBBox :: RItem -> BBox 
rItemBBox (RItemStroke strk) = strkbbx_bbx strk
rItemBBox (RItemImage img _) = imgbbx_bbx img

-}
