{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Hoodle.Simple where

import Data.Aeson.TH (defaultOptions, deriveJSON, fieldLabelModifier)
import Data.Aeson.Types
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
    object,
    (.:),
    (.=),
  )
import Data.ByteString.Char8 (ByteString, pack)
import Data.Hoodle.Util (fst3, snd3)
import qualified Data.Serialize as SE
import Data.Strict.Tuple (Pair (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID.V4 (nextRandom)
import Lens.Micro (Lens', lens, set)
import Prelude hiding (curry, fst, putStrLn, snd, uncurry)

-- |
type Title = ByteString

-- | Orphan instance for ByteString
-- __TODO__: Remove this
instance ToJSON ByteString where
  toJSON = String . TE.decodeUtf8

-- | Orphan instance for ByteString
-- __TODO__: Remove this
instance FromJSON ByteString where
  parseJSON v = TE.encodeUtf8 <$> parseJSON v

-- | Orphan instance for Pair
instance (SE.Serialize a, SE.Serialize b) => SE.Serialize (Pair a b) where
  put (x :!: y) =
    SE.put x
      >> SE.put y
  get = (:!:) <$> SE.get <*> SE.get

-- |
data Dimension = Dim {dim_width :: !Double, dim_height :: !Double}
  deriving (Show, Eq, Ord)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''Dimension)

-- |
instance SE.Serialize Dimension where
  put (Dim w h) = SE.put w >> SE.put h
  get = Dim <$> SE.get <*> SE.get

-- | Pen stroke item
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

$(deriveJSON defaultOptions {fieldLabelModifier = drop 7} ''Stroke)

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

-- | Image item
data Image = Image
  { img_src :: ByteString,
    img_pos :: (Double, Double),
    img_dim :: !Dimension
  }
  deriving (Show, Eq, Ord)

instance ToJSON Image where
  toJSON Image {..} =
    object
      [ "pos" .= toJSON img_pos,
        "dim" .= toJSON img_dim
      ]

instance FromJSON Image where
  parseJSON (Object v) = Image "" <$> v .: "pos" <*> v .: "dim"
  parseJSON _ = fail "error in parsing Image"

--   $(deriveJSON defaultOptions {fieldLabelModifier = drop 4}  ''Image)

instance SE.Serialize Image where
  put Image {..} =
    SE.put img_src
      >> SE.put img_pos
      >> SE.put img_dim
  get = Image <$> SE.get <*> SE.get <*> SE.get

data SVG = SVG
  { svg_text :: Maybe ByteString,
    svg_command :: Maybe ByteString,
    svg_render :: ByteString,
    svg_pos :: (Double, Double),
    svg_dim :: !Dimension
  }
  deriving (Show, Eq, Ord)

--  $(deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''SVG)

instance ToJSON SVG where
  toJSON SVG {..} =
    object
      [ "pos" .= toJSON svg_pos,
        "dim" .= toJSON svg_dim
      ]

instance FromJSON SVG where
  parseJSON (Object v) = SVG Nothing Nothing "" <$> v .: "pos" <*> v .: "dim"
  parseJSON _ = fail "error in parsing SVG"

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
data Link
  = Link
      { link_id :: ByteString,
        link_type :: ByteString,
        link_location :: ByteString,
        link_text :: Maybe ByteString,
        link_command :: Maybe ByteString,
        link_render :: ByteString,
        link_pos :: (Double, Double),
        link_dim :: !Dimension
      }
  | LinkDocID
      { link_id :: ByteString,
        link_linkeddocid :: ByteString,
        link_location :: ByteString,
        link_text :: Maybe ByteString,
        link_command :: Maybe ByteString,
        link_render :: ByteString,
        link_pos :: (Double, Double),
        link_dim :: !Dimension
      }
  | LinkAnchor
      { link_id :: ByteString,
        link_linkeddocid :: ByteString,
        link_location :: ByteString,
        link_anchorid :: ByteString,
        link_render :: ByteString,
        link_pos :: (Double, Double),
        link_dim :: !Dimension
      }
  deriving (Show, Eq, Ord)

instance ToJSON Link where
  toJSON Link {..} =
    object
      [ "tag" .= String "Link",
        "id" .= toJSON link_id,
        "type" .= toJSON link_type,
        "location" .= toJSON link_location,
        "pos" .= toJSON link_pos,
        "dim" .= toJSON link_dim
      ]
  toJSON LinkDocID {..} =
    object
      [ "tag" .= String "LinkDocID",
        "id" .= toJSON link_id,
        "linkeddocid" .= toJSON link_linkeddocid,
        "location" .= toJSON link_location,
        "pos" .= toJSON link_pos,
        "dim" .= toJSON link_dim
      ]
  toJSON LinkAnchor {..} =
    object
      [ "tag" .= String "LinkAnchor",
        "id" .= toJSON link_id,
        "linkeddocid" .= toJSON link_linkeddocid,
        "location" .= toJSON link_location,
        "anchorid" .= toJSON link_anchorid,
        "pos" .= toJSON link_pos,
        "dim" .= toJSON link_dim
      ]

instance FromJSON Link where
  parseJSON (Object v) = do
    tag :: T.Text <- v .: "tag"
    case tag of
      "Link" ->
        Link <$> v .: "id" <*> v .: "type" <*> v .: "location"
          <*> pure Nothing
          <*> pure Nothing
          <*> pure ""
          <*> v
            .: "pos"
          <*> v
            .: "dim"
      "LinkDocID" ->
        LinkDocID <$> v .: "id" <*> v .: "linkeddocid"
          <*> v
          .: "location"
          <*> pure Nothing
          <*> pure Nothing
          <*> pure ""
          <*> v
          .: "pos"
          <*> v
          .: "dim"
      "LinkAnchor" ->
        LinkAnchor <$> v .: "id" <*> v .: "linkeddocid"
          <*> v
          .: "location"
          <*> v
          .: "anchorid"
          <*> pure ""
          <*> v
          .: "pos"
          <*> v
          .: "dim"
      _ -> fail "error in parsing Link"
  parseJSON _ = fail "error in parsing Link"

--  $(deriveJSON defaultOptions { fieldLabelModifier = drop 5 } ''Link)

instance SE.Serialize Link where
  put Link {..} =
    SE.putWord8 0
      >> SE.put link_id
      >> SE.put link_type
      >> SE.put link_location
      >> SE.put link_text
      >> SE.put link_command
      >> SE.put link_render
      >> SE.put link_pos
      >> SE.put link_dim
  put LinkDocID {..} =
    SE.putWord8 1
      >> SE.put link_id
      >> SE.put link_linkeddocid
      >> SE.put link_location
      >> SE.put link_text
      >> SE.put link_command
      >> SE.put link_render
      >> SE.put link_pos
      >> SE.put link_dim
  put LinkAnchor {..} =
    SE.putWord8 2
      >> SE.put link_id
      >> SE.put link_linkeddocid
      >> SE.put link_location
      >> SE.put link_anchorid
      >> SE.put link_render
      >> SE.put link_pos
      >> SE.put link_dim
  get = do
    tag <- SE.getWord8
    case tag of
      0 ->
        Link <$> SE.get <*> SE.get <*> SE.get <*> SE.get
          <*> SE.get
          <*> SE.get
          <*> SE.get
          <*> SE.get
      1 ->
        LinkDocID <$> SE.get <*> SE.get <*> SE.get <*> SE.get
          <*> SE.get
          <*> SE.get
          <*> SE.get
          <*> SE.get
      2 ->
        LinkAnchor <$> SE.get <*> SE.get <*> SE.get <*> SE.get
          <*> SE.get
          <*> SE.get
          <*> SE.get
      _ -> fail "err in Link parsing"

data Anchor = Anchor
  { anchor_id :: ByteString,
    anchor_render :: ByteString,
    anchor_pos :: (Double, Double),
    anchor_dim :: !Dimension
  }
  deriving (Show, Eq, Ord)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 7} ''Anchor)

instance SE.Serialize Anchor where
  put Anchor {..} =
    SE.put anchor_id
      >> SE.put anchor_render
      >> SE.put anchor_pos
      >> SE.put anchor_dim
  get = Anchor <$> SE.get <*> SE.get <*> SE.get <*> SE.get

-- | wrapper of object embeddable in Layer
data Item
  = ItemStroke Stroke
  | ItemImage Image
  | ItemSVG SVG
  | ItemLink Link
  | ItemAnchor Anchor
  deriving (Show, Eq, Ord)

$(deriveJSON defaultOptions ''Item)

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
  put (ItemLink lnk) =
    SE.putWord8 3
      >> SE.put lnk
  put (ItemAnchor anc) =
    SE.putWord8 4
      >> SE.put anc
  get = do
    tag <- SE.getWord8
    case tag of
      0 -> ItemStroke <$> SE.get
      1 -> ItemImage <$> SE.get
      2 -> ItemSVG <$> SE.get
      3 -> ItemLink <$> SE.get
      4 -> ItemAnchor <$> SE.get
      _ -> fail "err in Item parsing"

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
  | BackgroundEmbedPdf
      { bkg_type :: ByteString,
        bkg_pageno :: Int
      }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''Background)

-- |
data Revision
  = Revision
      { _revmd5 :: !ByteString,
        _revtxt :: !ByteString
      }
  | RevisionInk
      { _revmd5 :: !ByteString,
        _revink :: [Stroke]
      }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Revision)

-- |
newtype Layer = Layer {layer_items :: [Item]}
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 6} ''Layer)

-- |
data Page = Page
  { page_dim :: !Dimension,
    page_bkg :: !Background,
    page_layers :: ![Layer]
  }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 5} ''Page)

-- |
data Hoodle = Hoodle
  { hoodle_id :: ByteString,
    hoodle_title :: !Title,
    hoodle_revisions :: [Revision],
    hoodle_embeddedpdf :: Maybe ByteString,
    hoodle_embeddedtext :: Maybe T.Text,
    hoodle_pages :: ![Page]
  }
  deriving (Show)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 7} ''Hoodle)

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
hoodleID :: Lens' Hoodle ByteString
hoodleID = lens hoodle_id (\f a -> f {hoodle_id = a})

-- |
title :: Lens' Hoodle Title
title = lens hoodle_title (\f a -> f {hoodle_title = a})

-- |
revisions :: Lens' Hoodle [Revision]
revisions = lens hoodle_revisions (\f a -> f {hoodle_revisions = a})

-- |
revmd5 :: Lens' Revision ByteString
revmd5 = lens _revmd5 (\f a -> f {_revmd5 = a})

-- |
embeddedPdf :: Lens' Hoodle (Maybe ByteString)
embeddedPdf = lens hoodle_embeddedpdf (\f a -> f {hoodle_embeddedpdf = a})

-- |
embeddedText :: Lens' Hoodle (Maybe T.Text)
embeddedText = lens hoodle_embeddedtext (\f a -> f {hoodle_embeddedtext = a})

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
emptyHoodle :: IO Hoodle
emptyHoodle = do
  uuid <- nextRandom
  return $ Hoodle ((pack . show) uuid) "" [] Nothing Nothing []

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
defaultHoodle :: IO Hoodle
defaultHoodle =
  set title "untitled" . set embeddedPdf Nothing . set pages [defaultPage]
    <$> emptyHoodle

-- |
newPageFromOld :: Page -> Page
newPageFromOld page =
  Page
    { page_dim = page_dim page,
      page_bkg = page_bkg page,
      page_layers = [emptyLayer]
    }
