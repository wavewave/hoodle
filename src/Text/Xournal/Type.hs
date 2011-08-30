{-# LANGUAGE BangPatterns #-}

module Text.Xournal.Type where

import qualified Data.ByteString as S
import Data.Strict.Tuple

import Prelude hiding (fst,snd,curry,uncurry)

type Title = S.ByteString

data Stroke = Stroke { stroke_tool  :: !S.ByteString
                     , stroke_color :: !S.ByteString
                     , stroke_width :: !Double
                     , stroke_data  :: ![Pair Double Double]
                     }
            deriving Show

data Dimension = Dim { dim_width :: !Double, dim_height :: !Double }
               deriving Show

data Background = Background { bkg_type :: !S.ByteString 
                             , bkg_color :: !S.ByteString 
                             , bkg_style :: !S.ByteString 
                             }
                deriving Show 

data Xournal = Xournal { xoj_title :: !Title, xoj_pages :: ![Page] }
             deriving Show 
data Page = Page { page_dim :: !Dimension
                 , page_bkg :: !Background 
                 , page_layers :: ![Layer] }
          deriving Show 
data Layer = Layer { layer_strokes :: ![Stroke] } 
           deriving Show 
