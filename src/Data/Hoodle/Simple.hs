{-# LANGUAGE BangPatterns, OverloadedStrings,  TypeOperators, 
             TypeFamilies, FlexibleContexts, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Hoodle.Simple 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
----------------------------------------------------------------------------- 

module Data.Hoodle.Simple where

-- from other packages
import           Control.Applicative 
import           Control.Lens 
import qualified Data.ByteString as S
import           Data.ByteString.Char8 hiding (map)
-- import           Data.Label
import qualified Data.Serialize as SE
import           Data.Strict.Tuple
-- from this package
import           Data.Hoodle.Util
-- 
import Prelude hiding ((.),id,putStrLn,fst,snd,curry,uncurry)

-- | 
type Title = S.ByteString

-- | 
data Stroke = Stroke { stroke_tool  :: !S.ByteString
                     , stroke_color :: !S.ByteString
                     , stroke_width :: !Double
                     , stroke_data  :: ![Pair Double Double]
                     }
            | VWStroke { stroke_tool :: S.ByteString 
                       , stroke_color :: S.ByteString 
                       , stroke_vwdata :: [(Double,Double,Double)] 
                       }
            deriving (Show,Eq,Ord)

-- | 
instance SE.Serialize Stroke where
    put Stroke{..} = SE.putWord8 0 
                     >> SE.put stroke_tool 
                     >> SE.put stroke_color
                     >> SE.put stroke_width
                     >> SE.put stroke_data
    put VWStroke{..} = SE.putWord8 1 
                       >> SE.put stroke_tool 
                       >> SE.put stroke_color
                       >> SE.put stroke_vwdata
    get = do tag <- SE.getWord8  
             case tag of 
               0 -> Stroke <$> SE.get <*> SE.get <*> SE.get <*> SE.get
               1 -> VWStroke <$> SE.get <*> SE.get <*> SE.get
               _ -> fail "err in Stroke parsing"
-- |    
instance (SE.Serialize a, SE.Serialize b) => SE.Serialize (Pair a b) where
    put (x :!: y) = SE.put x
                    >> SE.put y
    get = (:!:) <$> SE.get <*> SE.get

    
-- | 
data Dimension = Dim { dim_width :: !Double, dim_height :: !Double }
               deriving Show

-- | 
data Background = Background { bkg_type :: !S.ByteString 
                             , bkg_color :: !S.ByteString 
                             , bkg_style :: !S.ByteString 
                             }
                | BackgroundPdf { bkg_type :: S.ByteString 
                                , bkg_domain :: Maybe S.ByteString
                                , bkg_filename :: Maybe S.ByteString
                                , bkg_pageno :: Int
                                }
                deriving Show 

-- | 
data Hoodle = Hoodle { hoodle_title :: !Title
                     , hoodle_pages :: ![Page] }
             deriving Show 

-- | 
data Page = Page { page_dim :: !Dimension
                 , page_bkg :: !Background 
                 , page_layers :: ![Layer] }
          deriving Show 

-- | 
data Layer = Layer { layer_strokes :: ![Stroke] } 
           deriving Show 

-- | 
getXYtuples :: Stroke -> [(Double,Double)]
getXYtuples (Stroke _t _c _w d) = map (\(x :!: y) -> (x,y)) d
getXYtuples (VWStroke _t _c d) = map ((,)<$>fst3<*>snd3) d 

----------------------------
-- Lenses
----------------------------

-- | 
s_tool :: Simple Lens Stroke ByteString
s_tool = lens stroke_tool (\f a -> f { stroke_tool = a })  

-- | 
s_color :: Simple Lens Stroke ByteString 
s_color = lens stroke_color (\f a -> f { stroke_color = a } )

-- | 
s_title :: Simple Lens Hoodle Title
s_title = lens hoodle_title (\f a -> f { hoodle_title = a } )

-- | 
s_pages :: Simple Lens Hoodle [Page]
s_pages = lens hoodle_pages (\f a -> f { hoodle_pages = a } )

-- | 
s_dim :: Simple Lens Page Dimension 
s_dim = lens page_dim (\f a -> f { page_dim = a } )

-- | 
s_bkg :: Simple Lens Page Background 
s_bkg = lens page_bkg (\f a -> f { page_bkg = a } )

-- | 
s_layers :: Simple Lens Page [Layer] 
s_layers = lens page_layers (\f a -> f { page_layers = a } )

-- | 
s_strokes :: Simple Lens Layer [Stroke]
s_strokes = lens layer_strokes (\f a -> f { layer_strokes = a } )

--------------------------
-- empty objects
--------------------------

-- | 
emptyHoodle :: Hoodle
emptyHoodle = Hoodle "" [] 

-- | 
emptyLayer :: Layer 
emptyLayer = Layer { layer_strokes = [] }

-- | 
emptyStroke :: Stroke 
emptyStroke = Stroke "pen" "black" 1.4 []

-- | 
defaultBackground :: Background
defaultBackground = Background { bkg_type = "solid"
                               , bkg_color = "white"
                               , bkg_style = "lined" 
                               }

-- | 
defaultLayer :: Layer
defaultLayer = Layer { layer_strokes  = [] } 

-- | 
defaultPage :: Page
defaultPage = Page { page_dim = Dim  612.0 792.0 
                   , page_bkg = defaultBackground
                   , page_layers = [ defaultLayer ] 
                   } 

-- | 
defaultHoodle :: Hoodle 
defaultHoodle = Hoodle "untitled" [ defaultPage  ] 

-- | 
newPageFromOld :: Page -> Page
newPageFromOld page = 
  Page { page_dim = page_dim page 
       , page_bkg = page_bkg page 
       , page_layers = [emptyLayer] } 
                   

