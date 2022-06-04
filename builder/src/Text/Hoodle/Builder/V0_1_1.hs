{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Hoodle.Builder.V0_1_1 where

import Blaze.ByteString.Builder (Builder, fromByteString, toLazyByteString)
import Blaze.ByteString.Builder.Char8 (fromChar, fromString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Double.Conversion.ByteString (toFixed)
import Data.Hoodle.Simple.V0_1_1
  ( Background (..),
    Dimension (..),
    Hoodle (..),
    Image (..),
    Item (..),
    Layer (..),
    Page (..),
    SVG (..),
    Stroke (..),
    background,
    dimension,
    items,
    layers,
    pages,
    title,
  )
import Data.Strict.Tuple (Pair (..))
import Lens.Micro.Extras (view)

-- |
builder :: Hoodle -> L.ByteString
builder = toLazyByteString . buildHoodle

-- |
buildHoodle :: Hoodle -> Builder
buildHoodle hdl =
  fromByteString "<?xml version=\"1.0\" standalone=\"no\"?>\n<hoodle version=\"0.1.1\">\n"
    <> (buildTitle . view title) hdl
    <> (mconcat . map buildPage . view pages) hdl
    <> fromByteString "</hoodle>\n"

-- |
buildTitle :: S.ByteString -> Builder
buildTitle ttl =
  fromByteString "<title>"
    <> fromByteString ttl
    <> fromByteString "</title>\n"

-- |
buildPage :: Page -> Builder
buildPage pg =
  fromByteString "<page width=\""
    <> fromByteString (toFixed 2 w)
    <> fromByteString "\" height=\""
    <> fromByteString (toFixed 2 h)
    <> fromByteString "\">\n"
    <> (buildBackground . view background) pg
    <> (mconcat . map buildLayer . view layers) pg
    <> fromByteString "</page>\n"
  where
    Dim w h = view dimension pg

-- |
buildBackground :: Background -> Builder
buildBackground bkg =
  case bkg of
    Background typ col sty ->
      fromByteString "<background type=\""
        <> fromByteString typ
        <> fromByteString "\" color=\""
        <> fromByteString col
        <> fromByteString "\" style=\""
        <> fromByteString sty
        <> fromByteString "\"/>\n"
    BackgroundPdf typ mdom mfile pageno ->
      fromByteString "<background type=\""
        <> fromByteString typ
        <> case mdom of
          Nothing -> fromByteString S.empty
          Just dom ->
            fromByteString "\" domain=\""
              <> fromByteString dom
        <> case mfile of
          Nothing -> fromByteString S.empty
          Just file ->
            fromByteString "\" filename=\""
              <> fromByteString file
        <> fromByteString "\" pageno=\""
        <> fromString (show pageno)
        <> fromByteString "\"/>\n"

-- |
buildLayer :: Layer -> Builder
buildLayer layer =
  fromByteString "<layer>\n"
    <> (mconcat . map buildItem . view items) layer
    <> fromByteString "</layer>\n"

buildItem :: Item -> Builder
buildItem (ItemStroke strk) = buildStroke strk
buildItem (ItemImage img) = buildImage img
buildItem (ItemSVG svg) = buildSVG svg

-- |
buildStroke :: Stroke -> Builder
buildStroke stroke@Stroke {} =
  fromByteString "<stroke tool=\""
    <> fromByteString (stroke_tool stroke)
    <> fromByteString "\" color=\""
    <> fromByteString (stroke_color stroke)
    <> fromByteString "\" width=\""
    <> fromByteString (toFixed 2 (stroke_width stroke))
    <> fromByteString "\">\n"
    <> mconcat (map build2D (stroke_data stroke))
    <> fromByteString "\n</stroke>\n"
buildStroke stroke@VWStroke {} =
  fromByteString "<stroke tool=\""
    <> fromByteString (stroke_tool stroke)
    <> fromByteString "\" color=\""
    <> fromByteString (stroke_color stroke)
    <> fromByteString "\" width=\""
    <> mconcat (map buildZFrm3D (stroke_vwdata stroke))
    <> fromByteString "\">\n"
    <> mconcat (map buildXYFrm3D (stroke_vwdata stroke))
    <> fromByteString "\n</stroke>\n"

buildImage :: Image -> Builder
buildImage (Image bstr (x, y) (Dim w h)) =
  fromByteString "<img src=\""
    <> fromByteString bstr
    <> fromByteString "\" x=\""
    <> fromByteString (toFixed 2 x)
    <> fromByteString "\" y=\""
    <> fromByteString (toFixed 2 y)
    <> fromByteString "\" width=\""
    <> fromByteString (toFixed 2 w)
    <> fromByteString "\" height=\""
    <> fromByteString (toFixed 2 h)
    <> fromByteString "\" />\n"

buildSVG :: SVG -> Builder
buildSVG (SVG mtxt mcmd rdr (x, y) (Dim w h)) =
  fromByteString "<svgobject x=\""
    <> fromByteString (toFixed 2 x)
    <> fromByteString "\" y=\""
    <> fromByteString (toFixed 2 y)
    <> fromByteString "\" width=\""
    <> fromByteString (toFixed 2 w)
    <> fromByteString "\" height=\""
    <> fromByteString (toFixed 2 h)
    <> fromByteString "\" >\n"
    <> maybe mempty (\txt -> fromByteString "<text><![CDATA[" <> fromByteString txt <> fromByteString "]]></text>") mtxt
    <> maybe mempty (\cmd -> fromByteString "<command><![CDATA[" <> fromByteString cmd <> fromByteString "]]></command>") mcmd
    <> fromByteString "<render><![CDATA["
    <> fromByteString rdr
    <> fromByteString "]]></render>"
    <> fromByteString "</svgobject>"

-- |
build2D :: Pair Double Double -> Builder
build2D (x :!: y) =
  fromByteString (toFixed 2 x)
    <> fromChar ' '
    <> fromByteString (toFixed 2 y)
    <> fromChar ' '

-- |
buildXYFrm3D :: (Double, Double, Double) -> Builder
buildXYFrm3D (x, y, _) =
  fromByteString (toFixed 2 x)
    <> fromChar ' '
    <> fromByteString (toFixed 2 y)
    <> fromChar ' '

-- |
buildZFrm3D :: (Double, Double, Double) -> Builder
buildZFrm3D (_, _, z) =
  fromByteString (toFixed 2 z)
    <> fromChar ' '
