{-# LANGUAGE BangPatterns, OverloadedStrings,  TypeOperators, 
             TypeFamilies, FlexibleContexts, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Hoodle.Simple 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
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
import           Data.ByteString.Char8 hiding (map)
import           Data.UUID.V4 
import qualified Data.Serialize as SE
import           Data.Strict.Tuple
-- from this package
import           Data.Hoodle.Util
-- 
import Prelude hiding (putStrLn,fst,snd,curry,uncurry)

-- | 
type Title = ByteString

-- | wrapper of object embeddable in Layer
data Item = ItemStroke Stroke
          | ItemImage Image
          | ItemSVG SVG
          | ItemLink Link
          | ItemAnchor Anchor
          deriving (Show,Eq,Ord)


-- | Pen stroke item 
data Stroke = Stroke { stroke_tool  :: !ByteString
                     , stroke_color :: !ByteString
                     , stroke_width :: !Double
                     , stroke_data  :: ![Pair Double Double]
                     }
            | VWStroke { stroke_tool :: ByteString 
                       , stroke_color :: ByteString 
                       , stroke_vwdata :: [(Double,Double,Double)] 
                       }
            deriving (Show,Eq,Ord)

-- | Image item 
data Image = Image { img_src :: ByteString
                   , img_pos :: (Double,Double)
                   , img_dim :: !Dimension
                   } 
             deriving (Show,Eq,Ord)

data SVG = SVG { svg_text :: Maybe ByteString
               , svg_command :: Maybe ByteString 
               , svg_render :: ByteString
               , svg_pos :: (Double,Double)
               , svg_dim :: !Dimension }  
           deriving (Show,Eq,Ord)
                    

data Link = Link { link_id :: ByteString 
                 , link_type :: ByteString 
                 , link_location :: ByteString
                 , link_text :: Maybe ByteString
                 , link_command :: Maybe ByteString 
                 , link_render :: ByteString
                 , link_pos :: (Double,Double)
                 , link_dim :: !Dimension }  
          | LinkDocID { link_id :: ByteString
                      , link_linkeddocid :: ByteString 
                      , link_location :: ByteString 
                      , link_text :: Maybe ByteString
                      , link_command :: Maybe ByteString 
                      , link_render :: ByteString
                      , link_pos :: (Double,Double)
                      , link_dim :: !Dimension }  
          | LinkAnchor { link_id :: ByteString 
                       , link_linkeddocid :: ByteString
                       , link_location :: ByteString
                       , link_anchorid :: ByteString
                       , link_pos :: (Double,Double)
                       , link_dim :: !Dimension
                       }

           deriving (Show,Eq,Ord)                    

data Anchor = Anchor { anchor_id :: ByteString 
                     , anchor_pos :: (Double, Double)
                     , anchor_dim :: !Dimension
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
instance SE.Serialize Image where
    put Image {..} = SE.put img_src
                     >> SE.put img_pos
                     >> SE.put img_dim
    get = Image <$> SE.get <*> SE.get <*> SE.get

-- | 
instance SE.Serialize SVG where
    put SVG {..} = SE.put svg_text
                   >> SE.put svg_command 
                   >> SE.put svg_render
                   >> SE.put svg_pos
                   >> SE.put svg_dim
    get = SVG <$> SE.get <*> SE.get <*> SE.get <*> SE.get <*> SE.get
    
    

-- | 
instance SE.Serialize Link where
    put Link {..}       = SE.putWord8 0 
                          >> SE.put link_id
                          >> SE.put link_type
                          >> SE.put link_location
                          >> SE.put link_text
                          >> SE.put link_command 
                          >> SE.put link_render
                          >> SE.put link_pos
                          >> SE.put link_dim
    put LinkDocID {..}  = SE.putWord8 1 
                          >> SE.put link_id
                          >> SE.put link_linkeddocid 
                          >> SE.put link_location
                          >> SE.put link_text
                          >> SE.put link_command 
                          >> SE.put link_render
                          >> SE.put link_pos
                          >> SE.put link_dim
    put LinkAnchor {..} = SE.putWord8 2
                          >> SE.put link_id
                          >> SE.put link_linkeddocid
                          >> SE.put link_location
                          >> SE.put link_anchorid
                          >> SE.put link_pos
                          >> SE.put link_dim
    get = do tag <- SE.getWord8 
             case tag of 
               0 -> Link       <$> SE.get <*> SE.get <*> SE.get <*> SE.get 
                               <*> SE.get <*> SE.get <*> SE.get <*> SE.get
               1 -> LinkDocID  <$> SE.get <*> SE.get <*> SE.get <*> SE.get 
                               <*> SE.get <*> SE.get <*> SE.get <*> SE.get
               2 -> LinkAnchor <$> SE.get <*> SE.get <*> SE.get <*> SE.get
                               <*> SE.get <*> SE.get
               _ -> fail "err in Link parsing"



instance SE.Serialize Anchor where
    put Anchor {..} = SE.put anchor_id
                      >> SE.put anchor_pos
                      >> SE.put anchor_dim
    get = Anchor <$> SE.get <*> SE.get <*> SE.get


-- | 
instance SE.Serialize Item where
    put (ItemStroke str) = SE.putWord8 0 
                           >> SE.put str 
    put (ItemImage img) = SE.putWord8 1 
                          >> SE.put img
    put (ItemSVG svg) = SE.putWord8 2
                        >> SE.put svg 
    put (ItemLink lnk) = SE.putWord8 3
                         >> SE.put lnk                         
    put (ItemAnchor anc) = SE.putWord8 4
                           >> SE.put anc                         
    get = do tag <- SE.getWord8 
             case tag of 
               0 -> ItemStroke <$> SE.get
               1 -> ItemImage <$> SE.get
               2 -> ItemSVG <$> SE.get
               3 -> ItemLink <$> SE.get
               4 -> ItemAnchor <$> SE.get
               _ -> fail "err in Item parsing"

-- |    
instance (SE.Serialize a, SE.Serialize b) => SE.Serialize (Pair a b) where
    put (x :!: y) = SE.put x
                    >> SE.put y
    get = (:!:) <$> SE.get <*> SE.get

    
-- | 
data Dimension = Dim { dim_width :: !Double, dim_height :: !Double }
               deriving (Show,Eq,Ord)

-- | 
instance SE.Serialize Dimension where 
  put (Dim w h) = SE.put w >> SE.put h 
  get = Dim <$> SE.get <*> SE.get

-- | 
data Background = Background { bkg_type :: !ByteString 
                             , bkg_color :: !ByteString 
                             , bkg_style :: !ByteString 
                             }
                | BackgroundPdf { bkg_type :: ByteString 
                                , bkg_domain :: Maybe ByteString
                                , bkg_filename :: Maybe ByteString
                                , bkg_pageno :: Int
                                }
                | BackgroundEmbedPdf { bkg_type :: ByteString
                                     , bkg_pageno :: Int } 
                deriving Show 
-- | 
data Revision = Revision { _revmd5 :: !ByteString 
                         , _revtxt :: !ByteString 
                         }  
              | RevisionInk { _revmd5 :: !ByteString
                            , _revink :: [Stroke] } 
              deriving Show 

-- | 
data Hoodle = Hoodle { hoodle_id :: ByteString
                     , hoodle_title :: !Title
                     , hoodle_revisions :: [Revision]
                     , hoodle_embeddedpdf :: Maybe ByteString
                     , hoodle_pages :: ![Page] }
             deriving Show 

-- | 
data Page = Page { page_dim :: !Dimension
                 , page_bkg :: !Background 
                 , page_layers :: ![Layer] }
          deriving Show 

-- | 
data Layer = Layer { layer_items :: ![Item] } 
           deriving Show 

-- | 
getXYtuples :: Stroke -> [(Double,Double)]
getXYtuples (Stroke _t _c _w d) = map (\(x :!: y) -> (x,y)) d
getXYtuples (VWStroke _t _c d) = map ((,)<$>fst3<*>snd3) d 


----------------------------
-- Lenses
----------------------------

-- | 
tool :: Simple Lens Stroke ByteString
tool = lens stroke_tool (\f a -> f { stroke_tool = a })  

-- | 
color :: Simple Lens Stroke ByteString 
color = lens stroke_color (\f a -> f { stroke_color = a } )

-- |
hoodleID :: Simple Lens Hoodle ByteString 
hoodleID = lens hoodle_id (\f a -> f { hoodle_id = a } )

-- | 
title :: Simple Lens Hoodle Title
title = lens hoodle_title (\f a -> f { hoodle_title = a } )

-- | 
revisions :: Simple Lens Hoodle [Revision]
revisions = lens hoodle_revisions (\f a -> f { hoodle_revisions = a } )

-- | 
revmd5 :: Simple Lens Revision ByteString
revmd5 = lens _revmd5 (\f a -> f { _revmd5 = a } )

-- | 
embeddedPdf :: Simple Lens Hoodle (Maybe ByteString)
embeddedPdf = lens hoodle_embeddedpdf (\f a -> f { hoodle_embeddedpdf = a} )

-- | 
pages :: Simple Lens Hoodle [Page]
pages = lens hoodle_pages (\f a -> f { hoodle_pages = a } )


-- | 
dimension :: Simple Lens Page Dimension 
dimension = lens page_dim (\f a -> f { page_dim = a } )

-- | 
background :: Simple Lens Page Background 
background = lens page_bkg (\f a -> f { page_bkg = a } )

-- | 
layers :: Simple Lens Page [Layer] 
layers = lens page_layers (\f a -> f { page_layers = a } )

-- | 
items :: Simple Lens Layer [Item]
items = lens layer_items (\f a -> f { layer_items = a } )


--------------------------
-- empty objects
--------------------------

-- | 
emptyHoodle :: IO Hoodle
emptyHoodle = do
  uuid <- nextRandom
  return $ Hoodle ((pack.show) uuid) "" [] Nothing [] 

-- | 
emptyLayer :: Layer 
emptyLayer = Layer { layer_items = [] } 

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
defaultPage :: Page
defaultPage = Page { page_dim = Dim  612.0 792.0 
                   , page_bkg = defaultBackground
                   , page_layers = [ emptyLayer ] 
                   } 

-- | 
defaultHoodle :: IO Hoodle 
defaultHoodle = 
    (set title "untitled".set embeddedPdf Nothing . set pages [defaultPage])
    <$> emptyHoodle 

-- | 
newPageFromOld :: Page -> Page
newPageFromOld page = 
  Page { page_dim = page_dim page 
       , page_bkg = page_bkg page 
       , page_layers = [emptyLayer] } 
                   

