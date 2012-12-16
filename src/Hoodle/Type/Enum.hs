{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Enum 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Type.Enum where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import           Data.Maybe 
import           Numeric (showHex)
-- 
import           Data.Hoodle.Predefined

-- | page add direction

data AddDirection = PageBefore | PageAfter
                  deriving (Show,Eq,Ord,Enum)

-- | relative zoom mode 

data ZoomModeRel = ZoomIn | ZoomOut
                 deriving (Show,Eq,Ord,Enum)

-- | 
data PenType = PenWork 
             | HighlighterWork 
             | EraserWork 
             | TextWork 
             deriving (Show,Eq,Ord)

-- | 
data PenColor = ColorBlack
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
              deriving (Show,Eq,Ord)

penColorNameMap :: M.Map PenColor B.ByteString                        
penColorNameMap = M.fromList [ (ColorBlack, "black")
                             , (ColorBlue , "blue")
                             , (ColorRed  , "red") 
                             , (ColorGreen, "green")
                             , (ColorGray,  "gray")
                             , (ColorLightBlue, "lightblue")
                             , (ColorLightGreen, "lightgreen")
                             , (ColorMagenta, "magenta")
                             , (ColorOrange, "orange")
                             , (ColorYellow, "yellow")
                             , (ColorWhite, "white") ]

penColorRGBAmap :: M.Map PenColor (Double,Double,Double,Double)
penColorRGBAmap = M.fromList $ map (\x->(fst x,fromJust (M.lookup (snd x) predefined_pencolor))) 
                             $ M.toList penColorNameMap 

convertPenColorToRGBA :: PenColor -> (Double,Double,Double,Double)
convertPenColorToRGBA (ColorRGBA r g b a) = (r,g,b,a)
convertPenColorToRGBA c = fromJust (M.lookup c penColorRGBAmap)

convertRGBAToHex :: (Double,Double,Double,Double) -> B.ByteString 
convertRGBAToHex (r,g,b,a) = B.pack ( ("#"++) 
                                      . hexify r  
                                      . hexify g
                                      . hexify b
                                      . hexify a $ "" )


hexify :: Double -> ShowS 
hexify x = if x >= 1.0 
             then ("ff"++)  
             else showHex (floor (x*256.0) :: Int)

convertPenColorToByteString :: PenColor-> B.ByteString 
convertPenColorToByteString pcol = 
    let mpcolname = M.lookup pcol penColorNameMap 
        pcolname = case mpcolname of 
                     Nothing -> (convertRGBAToHex . convertPenColorToRGBA) pcol
                     Just n -> n
    in pcolname 
