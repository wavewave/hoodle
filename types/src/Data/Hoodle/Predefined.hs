{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : Data.Hoodle.Predefined
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
module Data.Hoodle.Predefined where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Text.Printf

-- |
predefinedPenShapeAspectXY :: (Double, Double)
predefinedPenShapeAspectXY = (cos (pi / 6.0), sin (pi / 6.0))

-- |
hexToRGBA :: Integer -> (Double, Double, Double, Double)
hexToRGBA n =
  let r = n `div` (256 * 256 * 256)
      g = (n - r * 256 * 256 * 256) `div` (256 * 256)
      b = (n - r * 256 * 256 * 256 - g * 256 * 256) `div` 256
      a = n - r * 256 * 256 * 256 - g * 256 * 256 - b * 256
   in (fromIntegral r / 255.0, fromIntegral g / 255.0, fromIntegral b / 255.0, fromIntegral a / 255.0)

rgbaToHEX :: (Double, Double, Double, Double) -> String
rgbaToHEX (r, g, b, a) =
  let i :: Integer = floor (255 * a) + 256 * floor (255 * b) + 256 * 256 * floor (255 * g) + 256 * 256 * 256 * floor (255 * r)
   in printf "#%08x" i

predefinedHighlighterOpacity :: Double
predefinedHighlighterOpacity = 0.5

predefinedPencolor :: M.Map B.ByteString (Double, Double, Double, Double)
predefinedPencolor =
  M.fromList
    [ ("black", hexToRGBA 0x000000ff),
      ("blue", hexToRGBA 0x3333ccff),
      ("red", hexToRGBA 0xff0000ff),
      ("green", hexToRGBA 0x008000ff),
      ("gray", hexToRGBA 0x808080ff),
      ("lightblue", hexToRGBA 0x00c0ffff),
      ("lightgreen", hexToRGBA 0x00ff00ff),
      ("magenta", hexToRGBA 0xff00ffff),
      ("orange", hexToRGBA 0xff8000ff),
      ("yellow", hexToRGBA 0xffff00ff),
      ("white", hexToRGBA 0xffffffff)
    ]

-- | need to be refined.
getPenColor :: B.ByteString -> Maybe (Double, Double, Double, Double)
getPenColor b | (not . B.null) b =
  case B.head b of
    '#' -> Just (hexToRGBA . (read :: String -> Integer) . B.unpack $ ("0x" `B.append` B.tail b))
    _ -> M.lookup b predefinedPencolor
getPenColor _ = Nothing

-- |
predefinedBkgcolor :: M.Map B.ByteString (Double, Double, Double, Double)
predefinedBkgcolor =
  M.fromList
    [ ("", hexToRGBA 0xffffffff),
      ("blue", hexToRGBA 0xa0e8ffff),
      ("pink", hexToRGBA 0xffc0d4ff),
      ("green", hexToRGBA 0x80ffc0ff),
      ("orange", hexToRGBA 0xffc080ff),
      ("yellow", hexToRGBA 0xffff80ff),
      ("white", hexToRGBA 0xffffffff)
    ]

predefinedVeryfine :: Double
predefinedVeryfine = 0.42

predefinedFine :: Double
predefinedFine = 0.85

predefinedMedium :: Double
predefinedMedium = 1.41

predefinedThick :: Double
predefinedThick = 2.26

predefinedVerythick :: Double
predefinedVerythick = 5.67

predefinedUltrathick :: Double
predefinedUltrathick = 15.41

---- for Highlighter

predefinedHighlighterVeryfine :: Double
predefinedHighlighterVeryfine = 2.83

predefinedHighlighterFine :: Double
predefinedHighlighterFine = 2.83

predefinedHighlighterMedium :: Double
predefinedHighlighterMedium = 8.50

predefinedHighlighterThick :: Double
predefinedHighlighterThick = 19.84

predefinedHighlighterVerythick :: Double
predefinedHighlighterVerythick = 19.84

predefinedHighlighterUltrathick :: Double
predefinedHighlighterUltrathick = 30.84

---- for Eraser

predefinedEraserVeryfine :: Double
predefinedEraserVeryfine = 2.83

predefinedEraserFine :: Double
predefinedEraserFine = 2.83

predefinedEraserMedium :: Double
predefinedEraserMedium = 8.50

predefinedEraserThick :: Double
predefinedEraserThick = 19.84

predefinedEraserVerythick :: Double
predefinedEraserVerythick = 19.84

predefinedEraserUltrathick :: Double
predefinedEraserUltrathick = 30.84

predefinedRulingMarginColor :: (Double, Double, Double, Double)
predefinedRulingMarginColor = hexToRGBA 0xff0080ff

predefinedRulingColor :: (Double, Double, Double, Double)
predefinedRulingColor = hexToRGBA 0x40a0ffff

predefinedRulingThickness :: Double
predefinedRulingThickness = 0.5

predefinedRulingLeftMargin :: Double
predefinedRulingLeftMargin = 72.0

predefinedRulingTopMargin :: Double
predefinedRulingTopMargin = 80.0

predefinedRulingSpacing :: Double
predefinedRulingSpacing = 24.0

predefinedRulingBottomMargin :: Double
predefinedRulingBottomMargin = predefinedRulingSpacing

predefinedRulingGraphSpacing :: Double
predefinedRulingGraphSpacing = 14.17
