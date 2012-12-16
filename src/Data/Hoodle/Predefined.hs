{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Hoodle.Predefined 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Data.Hoodle.Predefined where

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

import Text.Printf 

-- | 

predefinedPenShapeAspectXY :: (Double,Double)
predefinedPenShapeAspectXY = (cos (pi/6.0), sin (pi/6.0))

-- | 

hexToRGBA :: Integer -> (Double,Double,Double,Double) 
hexToRGBA n = 
  let r = n `div` (256*256*256)
      g = (n-r*256*256*256) `div` (256*256)
      b = (n-r*256*256*256-g*256*256) `div` 256 
      a = n-r*256*256*256-g*256*256-b*256  
  in  (fromIntegral r/255.0,fromIntegral g/255.0,fromIntegral b/255.0,fromIntegral a/255.0)

rgbaToHEX :: (Double,Double,Double,Double) -> String
rgbaToHEX (r,g,b,a) = 
  let i :: Integer = floor (255*a) + 256*floor (255*b) + 256*256*floor (255*g) + 256*256*256*floor (255*r) 
  in printf "#%08x" i

predefined_highlighter_opacity :: Double 
predefined_highlighter_opacity = 0.5

predefined_pencolor :: M.Map B.ByteString (Double,Double,Double,Double)
predefined_pencolor = 
  M.fromList [ ("black"     , hexToRGBA 0x000000ff)
             , ("blue"      , hexToRGBA 0x3333ccff)
             , ("red"       , hexToRGBA 0xff0000ff)
             , ("green"     , hexToRGBA 0x008000ff)
             , ("gray"      , hexToRGBA 0x808080ff)
             , ("lightblue" , hexToRGBA 0x00c0ffff)
             , ("lightgreen", hexToRGBA 0x00ff00ff)
             , ("magenta"   , hexToRGBA 0xff00ffff)
             , ("orange"    , hexToRGBA 0xff8000ff)
             , ("yellow"    , hexToRGBA 0xffff00ff)
             , ("white"     , hexToRGBA 0xffffffff) ] 

-- | need to be refined. 
getPenColor :: B.ByteString -> Maybe (Double,Double,Double,Double) 
getPenColor b | (not . B.null) b = 
  case B.head b of 
    '#' -> Just (hexToRGBA . (read  :: String -> Integer) . B.unpack $ ("0x" `B.append` (B.tail b) ))
    _ -> M.lookup b predefined_pencolor 

-- | 
predefined_bkgcolor :: M.Map B.ByteString (Double,Double,Double,Double)
predefined_bkgcolor = 
  M.fromList [ (""      , hexToRGBA 0xffffffff) 
             , ("blue"  , hexToRGBA 0xa0e8ffff)
             , ("pink"  , hexToRGBA 0xffc0d4ff)
             , ("green" , hexToRGBA 0x80ffc0ff)
             , ("orange", hexToRGBA 0xffc080ff)
             , ("yellow", hexToRGBA 0xffff80ff)
             , ("white" , hexToRGBA 0xffffffff) ]

predefined_veryfine :: Double
predefined_veryfine = 0.42 

predefined_fine :: Double
predefined_fine = 0.85

predefined_medium :: Double
predefined_medium = 1.41 

predefined_thick :: Double
predefined_thick = 2.26

predefined_verythick :: Double 
predefined_verythick = 5.67

predefined_ultrathick :: Double 
predefined_ultrathick = 15.41

---- for Highlighter

predefined_highlighter_veryfine :: Double
predefined_highlighter_veryfine = 2.83

predefined_highlighter_fine :: Double
predefined_highlighter_fine = 2.83

predefined_highlighter_medium :: Double
predefined_highlighter_medium = 8.50

predefined_highlighter_thick :: Double
predefined_highlighter_thick = 19.84

predefined_highlighter_verythick :: Double 
predefined_highlighter_verythick = 19.84

predefined_highlighter_ultrathick :: Double 
predefined_highlighter_ultrathick = 30.84


---- for Eraser

predefined_eraser_veryfine :: Double
predefined_eraser_veryfine = 2.83

predefined_eraser_fine :: Double
predefined_eraser_fine = 2.83

predefined_eraser_medium :: Double
predefined_eraser_medium = 8.50

predefined_eraser_thick :: Double
predefined_eraser_thick = 19.84

predefined_eraser_verythick :: Double 
predefined_eraser_verythick = 19.84

predefined_eraser_ultrathick :: Double 
predefined_eraser_ultrathick = 30.84



predefined_RULING_MARGIN_COLOR :: (Double,Double,Double,Double)
predefined_RULING_MARGIN_COLOR = hexToRGBA 0xff0080ff

predefined_RULING_COLOR :: (Double,Double,Double,Double)
predefined_RULING_COLOR = hexToRGBA 0x40a0ffff

predefined_RULING_THICKNESS :: Double
predefined_RULING_THICKNESS = 0.5

predefined_RULING_LEFTMARGIN :: Double
predefined_RULING_LEFTMARGIN = 72.0

predefined_RULING_TOPMARGIN :: Double
predefined_RULING_TOPMARGIN = 80.0

predefined_RULING_SPACING :: Double
predefined_RULING_SPACING = 24.0

predefined_RULING_BOTTOMMARGIN :: Double 
predefined_RULING_BOTTOMMARGIN = predefined_RULING_SPACING

predefined_RULING_GRAPHSPACING :: Double
predefined_RULING_GRAPHSPACING = 14.17
