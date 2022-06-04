{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Xournal.Simple where

import Data.ByteString.Char8 (ByteString)
import qualified Data.Serialize as SE
import Data.Strict.Tuple (Pair ((:!:)))
import Data.Xournal.Util (fst3, snd3)
import Lens.Micro (Lens', lens)
import Prelude hiding (curry, fst, id, putStrLn, snd, uncurry, (.))

-- |
type Title = ByteString

-- |
data Stroke
  = Stroke
      { stroke_tool :: !ByteString,
        stroke_color :: !ByteString,
        stroke_width :: !Double,
        stroke_data :: ![Pair Double Double]
      }
  | VWStroke
      { stroke_tool :: ByteString,
        stroke_color :: ByteString,
        stroke_vwdata :: [(Double, Double, Double)]
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
instance (SE.Serialize a, SE.Serialize b) => SE.Serialize (Pair a b) where
  put (x :!: y) =
    SE.put x
      >> SE.put y
  get = (:!:) <$> SE.get <*> SE.get

-- |
data Dimension = Dim {dim_width :: !Double, dim_height :: !Double}
  deriving (Show)

-- |
data Background
  = Background
      { bkg_type :: !ByteString,
        bkg_color :: !ByteString,
        bkg_style :: !ByteString
      }
  | BackgroundPdf
      { bkg_type :: ByteString,
        bkg_domain :: Maybe ByteString,
        bkg_filename :: Maybe ByteString,
        bkg_pageno :: Int
      }
  deriving (Show)

-- |
data Xournal = Xournal {xoj_title :: !Title, xoj_pages :: ![Page]}
  deriving (Show)

-- |
data Page = Page
  { page_dim :: !Dimension,
    page_bkg :: !Background,
    page_layers :: ![Layer]
  }
  deriving (Show)

-- |
data Layer = Layer {layer_strokes :: ![Stroke]}
  deriving (Show)

-- |
getXYtuples :: Stroke -> [(Double, Double)]
getXYtuples (Stroke _t _c _w d) = map (\(x :!: y) -> (x, y)) d
getXYtuples (VWStroke _t _c d) = map ((,) <$> fst3 <*> snd3) d

----------------------------
-- Lenses
----------------------------

-- |
s_tool :: Lens' Stroke ByteString
s_tool = lens stroke_tool (\f a -> f {stroke_tool = a})

-- |
s_color :: Lens' Stroke ByteString
s_color = lens stroke_color (\f a -> f {stroke_color = a})

-- |
s_title :: Lens' Xournal Title
s_title = lens xoj_title (\f a -> f {xoj_title = a})

-- |
s_pages :: Lens' Xournal [Page]
s_pages = lens xoj_pages (\f a -> f {xoj_pages = a})

-- |
s_dim :: Lens' Page Dimension
s_dim = lens page_dim (\f a -> f {page_dim = a})

-- |
s_bkg :: Lens' Page Background
s_bkg = lens page_bkg (\f a -> f {page_bkg = a})

-- |
s_layers :: Lens' Page [Layer]
s_layers = lens page_layers (\f a -> f {page_layers = a})

-- |
s_strokes :: Lens' Layer [Stroke]
s_strokes = lens layer_strokes (\f a -> f {layer_strokes = a})

--------------------------
-- empty objects
--------------------------

-- |
emptyXournal :: Xournal
emptyXournal = Xournal "" []

-- |
emptyLayer :: Layer
emptyLayer = Layer {layer_strokes = []}

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
defaultLayer :: Layer
defaultLayer = Layer {layer_strokes = []}

-- |
defaultPage :: Page
defaultPage =
  Page
    { page_dim = Dim 612.0 792.0,
      page_bkg = defaultBackground,
      page_layers = [defaultLayer]
    }

-- |
defaultXournal :: Xournal
defaultXournal = Xournal "untitled" [defaultPage]

-- |
newPageFromOld :: Page -> Page
newPageFromOld page =
  Page
    { page_dim = page_dim page,
      page_bkg = page_bkg page,
      page_layers = [emptyLayer]
    }
