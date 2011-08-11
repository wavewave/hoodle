{-# LANGUAGE BangPatterns #-}

module Text.Xournal.Type where

type Title = String 

data Stroke = Stroke { stroke_tool  :: String
                     , stroke_color :: String
                     , stroke_width :: Double
                     , stroke_data  :: [(Double,Double)]
                     }
            deriving Show

data Dimension = Dim { dim_width :: Double, dim_height :: Double }
               deriving Show

data Background = Background { bkg_type :: String 
                             , bkg_color :: String 
                             , bkg_style :: String 
                             }
                deriving Show 

data Xournal = Xournal { xoj_title :: Title, xoj_pages :: [Page] }
             deriving Show 
data Page = Page { page_dim :: Dimension
                 , page_bkg :: Background 
                 , page_layers :: [Layer] }
          deriving Show 
data Layer = Layer { layer_strokes :: [Stroke] } 
           deriving Show 
