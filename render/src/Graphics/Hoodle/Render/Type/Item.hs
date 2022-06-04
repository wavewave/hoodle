{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Hoodle.Render.Type.Item where

import Data.Hoodle.BBox
  ( BBoxed (..),
    GetBBoxable (..),
  )
import Data.Hoodle.Simple
  ( Anchor (..),
    Image (..),
    Item (..),
    Link (..),
    SVG (..),
    Stroke (..),
  )
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.SVG as RSVG

data RItem
  = RItemStroke (BBoxed Stroke)
  | RItemImage (BBoxed Image) (Maybe Cairo.Surface) -- UUID
  | RItemSVG (BBoxed SVG) (Maybe RSVG.SVG)
  | RItemLink (BBoxed Link) (Maybe RSVG.SVG)
  | RItemAnchor (BBoxed Anchor) (Maybe RSVG.SVG)

instance GetBBoxable RItem where
  getBBox (RItemStroke strk) = getBBox strk
  getBBox (RItemImage img _) = getBBox img
  getBBox (RItemSVG svg _) = getBBox svg
  getBBox (RItemLink lnk _) = getBBox lnk
  getBBox (RItemAnchor anc _) = getBBox anc

instance Show RItem where
  show (RItemStroke strk) = "RItemStroke " ++ show strk
  show (RItemImage img _) = "RItemImage " ++ show img
  show (RItemSVG svg _) = "RItemSVG " ++ show svg
  show (RItemLink lnk _) = "RItemLink " ++ show lnk
  show (RItemAnchor anc _) = "RItemAnchor " ++ show anc

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
isLinkInRItem :: RItem -> Bool
isLinkInRItem (RItemLink _ _) = True
isLinkInRItem _ = False

-- |
isAnchorInRItem :: RItem -> Bool
isAnchorInRItem (RItemAnchor _ _) = True
isAnchorInRItem _ = False

-- |
findStrkInRItem :: RItem -> Maybe (BBoxed Stroke)
findStrkInRItem (RItemStroke strk) = Just strk
findStrkInRItem _ = Nothing

-- |
findImgInRItem :: RItem -> Maybe (BBoxed Image)
findImgInRItem (RItemImage img _) = Just img
findImgInRItem _ = Nothing

-- |
findSVGInRItem :: RItem -> Maybe (BBoxed SVG)
findSVGInRItem (RItemSVG svg _) = Just svg
findSVGInRItem _ = Nothing

-- |
rItem2Item :: RItem -> Item
rItem2Item (RItemStroke strk) = (ItemStroke . bbxed_content) strk
rItem2Item (RItemImage img _) = (ItemImage . bbxed_content) img
rItem2Item (RItemSVG svg _) = (ItemSVG . bbxed_content) svg
rItem2Item (RItemLink lnk _) = (ItemLink . bbxed_content) lnk
rItem2Item (RItemAnchor anc _) = (ItemAnchor . bbxed_content) anc
