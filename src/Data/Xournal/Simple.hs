{-# LANGUAGE BangPatterns, OverloadedStrings,  TypeOperators, 
             TypeFamilies, FlexibleContexts  #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Xournal.Simple 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
 
module Data.Xournal.Simple where

import qualified Data.ByteString as S
import Data.ByteString.Char8
import Data.Strict.Tuple

import Control.Category
import Data.Label
import Prelude hiding ((.),id,putStrLn)



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
                | BackgroundPdf { bkg_type :: S.ByteString 
                                , bkg_domain :: Maybe S.ByteString
                                , bkg_filename :: Maybe S.ByteString
                                , bkg_pageno :: Int
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

s_tool :: Stroke :-> ByteString
s_tool = lens stroke_tool (\a f -> f { stroke_tool = a })  

s_color :: Stroke :-> ByteString 
s_color = lens stroke_color (\a f -> f { stroke_color = a } )

s_width :: Stroke :-> Double 
s_width = lens stroke_width (\a f -> f { stroke_width = a } )

s_data :: Stroke :-> [Pair Double Double] 
s_data = lens stroke_data (\a f -> f { stroke_data = a } )

s_title :: Xournal :-> Title
s_title = lens xoj_title (\a f -> f { xoj_title = a } )

s_pages :: Xournal :-> [Page]
s_pages = lens xoj_pages (\a f -> f { xoj_pages = a } )

s_dim :: Page :-> Dimension 
s_dim = lens page_dim (\a f -> f { page_dim = a } )

s_bkg :: Page :-> Background 
s_bkg = lens page_bkg (\a f -> f { page_bkg = a } )

s_layers :: Page :-> [Layer] 
s_layers = lens page_layers (\a f -> f { page_layers = a } )

s_strokes :: Layer :-> [Stroke]
s_strokes = lens layer_strokes (\a f -> f { layer_strokes = a } )



emptyXournal :: Xournal
emptyXournal = Xournal "" [] 

emptyLayer :: Layer 
emptyLayer = Layer { layer_strokes = [] }

emptyStroke :: Stroke 
emptyStroke = Stroke "pen" "black" 1.4 []


defaultBackground :: Background
defaultBackground = Background { bkg_type = "solid"
                               , bkg_color = "white"
                               , bkg_style = "lined" 
                               }

defaultLayer :: Layer
defaultLayer = Layer { layer_strokes  = [] } 

defaultPage :: Page
defaultPage = Page { page_dim = Dim  612.0 792.0 
                   , page_bkg = defaultBackground
                   , page_layers = [ defaultLayer ] 
                   } 

defaultXournal :: Xournal 
defaultXournal = Xournal "untitled" [ defaultPage  ] 


newPageFromOld :: Page -> Page
newPageFromOld page = 
  Page { page_dim = page_dim page 
       , page_bkg = page_bkg page 
       , page_layers = [emptyLayer] } 
                   


{-
instance IStroke Stroke where
  strokeTool = stroke_tool
  strokeColor = stroke_color
  strokeWidth = stroke_width
  strokeData = stroke_data
 
instance ILayer Layer where
  type TStroke Layer = Stroke
  layerStrokes = layer_strokes 

instance IPage Page where
  type TLayer Page = Layer 
  pageDim = page_dim 
  pageBkg = page_bkg 
  pageLayers = page_layers

instance IXournal Xournal where
  type TPage Xournal = Page 
  xournalPages = xoj_pages 
-}
