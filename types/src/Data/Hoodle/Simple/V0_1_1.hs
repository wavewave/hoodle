{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Hoodle.Simple.V0_1_1 where

import Control.Applicative
import qualified Data.ByteString as S
import Data.ByteString.Char8 hiding (map)
import Data.Hoodle.Util
import qualified Data.Serialize as SE
import Data.Strict.Tuple
import Lens.Micro
import Prelude hiding (curry, fst, id, putStrLn, snd, uncurry, (.))

-- |
type Title = S.ByteString

-- | wrapper of object embeddable in Layer
data Item
  = ItemStroke Stroke
  | ItemImage Image
  | ItemSVG SVG
  --  | ItemLink Link
  deriving (Show, Eq, Ord)

-- | Pen stroke item
data Stroke
  = Stroke
      { stroke_tool :: !S.ByteString,
        stroke_color :: !S.ByteString,
        stroke_width :: !Double,
        stroke_data :: ![Pair Double Double]
      }
  | VWStroke
      { stroke_tool :: S.ByteString,
        stroke_color :: S.ByteString,
        stroke_vwdata :: [(Double, Double, Double)]
      }
  deriving (Show, Eq, Ord)

-- | Image item
data Image = Image
  { img_src :: S.ByteString,
    img_pos :: (Double, Double),
    img_dim :: !Dimension
  }
  deriving (Show, Eq, Ord)

data SVG = SVG
  { svg_text :: Maybe S.ByteString,
    svg_command :: Maybe S.ByteString,
    svg_render :: S.ByteString,
    svg_pos :: (Double, Double),
    svg_dim :: !Dimension
  }
  deriving (Show, Eq, Ord)

-- |
instance SE.Serialize Stroke where
  put Stroke {..} =
    SE.putWord8 0
      >> SE.put stroke_tool
      >> SE.put stroke_color
      >> SE.put stroke_width
      >> SE.put stroke_data
  put VWStroke {..} =
    SE.putWord8 1
      >> SE.put stroke_tool
      >> SE.put stroke_color
      >> SE.put stroke_vwdata
  get = do
    tag <- SE.getWord8
    case tag of
      0 -> Stroke <$> SE.get <*> SE.get <*> SE.get <*> SE.get
      1 -> VWStroke <$> SE.get <*> SE.get <*> SE.get
      _ -> fail "err in Stroke parsing"

-- |
instance SE.Serialize Image where
  put Image {..} =
    SE.put img_src
      >> SE.put img_pos
      >> SE.put img_dim
  get = Image <$> SE.get <*> SE.get <*> SE.get

-- |
instance SE.Serialize SVG where
  put SVG {..} =
    SE.put svg_text
      >> SE.put svg_command
      >> SE.put svg_render
      >> SE.put svg_pos
      >> SE.put svg_dim
  get = SVG <$> SE.get <*> SE.get <*> SE.get <*> SE.get <*> SE.get

-- |
instance SE.Serialize Item where
  put (ItemStroke str) =
    SE.putWord8 0
      >> SE.put str
  put (ItemImage img) =
    SE.putWord8 1
      >> SE.put img
  put (ItemSVG svg) =
    SE.putWord8 2
      >> SE.put svg
  get = do
    tag <- SE.getWord8
    case tag of
      0 -> ItemStroke <$> SE.get
      1 -> ItemImage <$> SE.get
      2 -> ItemSVG <$> SE.get
      _ -> fail "err in Item parsing"

-- |
instance (SE.Serialize a, SE.Serialize b) => SE.Serialize (Pair a b) where
  put (x :!: y) =
    SE.put x
      >> SE.put y
  get = (:!:) <$> SE.get <*> SE.get

-- |
data Dimension = Dim {dim_width :: !Double, dim_height :: !Double}
  deriving (Show, Eq, Ord)

-- |
instance SE.Serialize Dimension where
  put (Dim w h) = SE.put w >> SE.put h
  get = Dim <$> SE.get <*> SE.get

-- |
data Background
  = Background
      { bkg_type :: !S.ByteString,
        bkg_color :: !S.ByteString,
        bkg_style :: !S.ByteString
      }
  | BackgroundPdf
      { bkg_type :: S.ByteString,
        bkg_domain :: Maybe S.ByteString,
        bkg_filename :: Maybe S.ByteString,
        bkg_pageno :: Int
      }
  deriving (Show)

-- |
data Hoodle = Hoodle
  { hoodle_title :: !Title,
    hoodle_pages :: ![Page]
  }
  deriving (Show)

-- |
data Page = Page
  { page_dim :: !Dimension,
    page_bkg :: !Background,
    page_layers :: ![Layer]
  }
  deriving (Show)

-- |
data Layer = Layer {layer_items :: ![Item]}
  deriving (Show)

-- |
getXYtuples :: Stroke -> [(Double, Double)]
getXYtuples (Stroke _t _c _w d) = map (\(x :!: y) -> (x, y)) d
getXYtuples (VWStroke _t _c d) = map ((,) <$> fst3 <*> snd3) d

----------------------------
-- Lenses
----------------------------

-- |
tool :: Lens' Stroke ByteString
tool = lens stroke_tool (\f a -> f {stroke_tool = a})

-- |
color :: Lens' Stroke ByteString
color = lens stroke_color (\f a -> f {stroke_color = a})

-- |
title :: Lens' Hoodle Title
title = lens hoodle_title (\f a -> f {hoodle_title = a})

-- |
pages :: Lens' Hoodle [Page]
pages = lens hoodle_pages (\f a -> f {hoodle_pages = a})

-- |
dimension :: Lens' Page Dimension
dimension = lens page_dim (\f a -> f {page_dim = a})

-- |
background :: Lens' Page Background
background = lens page_bkg (\f a -> f {page_bkg = a})

-- |
layers :: Lens' Page [Layer]
layers = lens page_layers (\f a -> f {page_layers = a})

-- |
items :: Lens' Layer [Item]
items = lens layer_items (\f a -> f {layer_items = a})

--------------------------
-- empty objects
--------------------------

-- |
emptyHoodle :: Hoodle
emptyHoodle = Hoodle "" []

-- |
emptyLayer :: Layer
emptyLayer = Layer {layer_items = []}

-- |
emptyStroke :: Stroke
emptyStroke = Stroke "pen" "black" 1.4 []

-- |
defaultBackground :: Background
defaultBackground =
  Background
    { bkg_type = "solid",
      bkg_color = "white",
      bkg_style = "lined"
    }

-- |
defaultPage :: Page
defaultPage =
  Page
    { page_dim = Dim 612.0 792.0,
      page_bkg = defaultBackground,
      page_layers = [emptyLayer]
    }

-- |
defaultHoodle :: Hoodle
defaultHoodle = Hoodle "untitled" [defaultPage]

-- |
newPageFromOld :: Page -> Page
newPageFromOld page =
  Page
    { page_dim = page_dim page,
      page_bkg = page_bkg page,
      page_layers = [emptyLayer]
    }
