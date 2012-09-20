{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Type.Select 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Type.Select where

-- from other packages
import Data.IntMap hiding (map)
-- from hoodle-platform 
import Data.Hoodle.BBox
import Data.Hoodle.Generic
import Data.Hoodle.Simple 
import Data.Hoodle.Select
import Data.Hoodle.Zipper
-- import Data.Hoodle.Map
-- from this package
import Graphics.Hoodle.Render.Type.Hoodle 
import Graphics.Hoodle.Render.Type.Background
import Graphics.Hoodle.Render.Type.HitTest



----------------------------
-- select state rendering --
----------------------------

type TLayerSelectBuf a = GLayer (BufTypeFromLayer a) TEitherAlterHitted (StrokeTypeFromLayer a) 


type family StrokeTypeFromLayer a  :: * 
     
type family BufTypeFromLayer a :: *
     
type instance BufTypeFromLayer (GLayer b s a) = b     
     
type instance StrokeTypeFromLayer RLayer = StrokeBBox 

data TLayerSelectInPageBuf s a = TLayerSelectInPageBuf
                               { gselectedlayerbuf :: TLayerSelectBuf a 
                               , gotherlayersbuf :: s a
                               }


-- -- | 
-- newtype LyBuf = LyBuf { mbuffer :: Maybe Surface } 


-- -- | 
-- type TPageBBoxMapPDFBuf = 
--   TPageBBoxMapBkgBuf BackgroundPDFDrawable LyBuf
  
-- -- |   
-- type THoodleBBoxMapPDFBuf = 
--   THoodleBBoxMapBkgBuf BackgroundPDFDrawable LyBuf
  
-- |
type TTempPageSelectPDFBuf = 
  GPage RBackground (TLayerSelectInPageBuf ZipperSelect) RLayer

-- | 
type TTempHoodleSelectPDFBuf = 
  GSelect (IntMap RPage) (Maybe (Int, TTempPageSelectPDFBuf))


{-
-- | deprecated 

type TTempPageSelect = GPage Background (TLayerSelectInPage []) TLayerBBox
                       

-- | deprecated 

type TTempHoodleSelect = GSelect (IntMap TPageBBoxMap) (Maybe (Int, TTempPageSelect))

-}


-- type TLayerSelect a = GLayer TEitherAlterHitted (StrokeTypeFromLayer a) 
