{-# LANGUAGE OverloadedStrings #-}

module Hoodle.Type.Enum where

import Control.Lens (Lens', lens)
import qualified Data.ByteString.Char8 as B
import Data.Hoodle.Predefined
  ( predefinedPencolor,
    rgbaToHEX,
  )
import qualified Data.Map as M
import Hoodle.Util (fromJustError)

-- | drawing efficiency
data DrawFlag = Clear | BkgEfficient | Efficient
  deriving (Eq, Ord, Show)

-- | page add direction
data AddDirection = PageBefore | PageAfter
  deriving (Show, Eq, Ord, Enum)

-- | relative zoom mode
data ZoomModeRel = ZoomIn | ZoomOut
  deriving (Show, Eq, Ord, Enum)

-- | pen tool type
data PenType
  = PenWork
  | HighlighterWork
  | EraserWork
  | VerticalSpaceWork
  deriving (Show, Eq, Ord)

-- | predefined pen colors
data PenColor
  = ColorBlack
  | ColorBlue
  | ColorRed
  | ColorGreen
  | ColorGray
  | ColorLightBlue
  | ColorLightGreen
  | ColorMagenta
  | ColorOrange
  | ColorYellow
  | ColorWhite
  | ColorRGBA Double Double Double Double
  deriving (Show, Eq, Ord)

-- | predefined background styles
data BackgroundStyle
  = BkgStylePlain
  | BkgStyleLined
  | BkgStyleRuled
  | BkgStyleGraph
  deriving (Show, Eq, Ord)

-- | mode for vertical space adding
data VerticalSpaceMode = GoingUp | GoingDown | OverPage

-- | select tool type
data SelectType
  = SelectLassoWork
  | SelectRectangleWork
  | SelectHandToolWork
  deriving (Show, Eq, Ord)

-- |
newtype SelectInfo = SelectInfo
  { _selectType :: SelectType
  }
  deriving (Show)

data RotateDir = CW | CCW deriving (Show, Ord, Eq)

-- |
data NewPageModeType = NPPlain | NPLast | NPCycle deriving (Show, Ord, Eq)

--
-- lenses
--

-- |
selectType :: Lens' SelectInfo SelectType
selectType = lens _selectType (\f a -> f {_selectType = a})

-- |
penColorNameMap :: M.Map PenColor B.ByteString
penColorNameMap =
  M.fromList
    [ (ColorBlack, "black"),
      (ColorBlue, "blue"),
      (ColorRed, "red"),
      (ColorGreen, "green"),
      (ColorGray, "gray"),
      (ColorLightBlue, "lightblue"),
      (ColorLightGreen, "lightgreen"),
      (ColorMagenta, "magenta"),
      (ColorOrange, "orange"),
      (ColorYellow, "yellow"),
      (ColorWhite, "white")
    ]

penColorRGBAmap :: M.Map PenColor (Double, Double, Double, Double)
penColorRGBAmap =
  M.fromList $
    map (\x -> (fst x, fromJustError "penColorRGBAmap" (M.lookup (snd x) predefinedPencolor))) $
      M.toList penColorNameMap

convertPenColorToRGBA :: PenColor -> (Double, Double, Double, Double)
convertPenColorToRGBA (ColorRGBA r g b a) = (r, g, b, a)
convertPenColorToRGBA c = fromJustError "convertPenColorToRGBA" (M.lookup c penColorRGBAmap)

convertRGBAToHex :: (Double, Double, Double, Double) -> B.ByteString
convertRGBAToHex = B.pack . rgbaToHEX

convertPenColorToByteString :: PenColor -> B.ByteString
convertPenColorToByteString pcol =
  let mpcolname = M.lookup pcol penColorNameMap
      pcolname = case mpcolname of
        Nothing -> (convertRGBAToHex . convertPenColorToRGBA) pcol
        Just n -> n
   in pcolname

convertBackgroundStyleToByteString :: BackgroundStyle -> B.ByteString
convertBackgroundStyleToByteString BkgStylePlain = "plain"
convertBackgroundStyleToByteString BkgStyleLined = "lined"
convertBackgroundStyleToByteString BkgStyleRuled = "ruled"
convertBackgroundStyleToByteString BkgStyleGraph = "graph"
