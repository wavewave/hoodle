o{-# LANGUAGE TypeFamilies #-}

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
import Data.Hoodle.Simple 
import Data.Hoodle.Generic
import Data.Hoodle.BBox
import Data.Hoodle.Map
-- from this package
import Graphics.Hoodle.Render.Type.HitTest

type TLayerSelectBuf a = GLayerBuf (BufTypeFromLayer a) TEitherAlterHitted (StrokeTypeFromLayer a) 

type family StrokeTypeFromLayer a  :: * 
     
type family BufTypeFromLayer a :: *
     
type instance BufTypeFromLayer (GLayerBuf b s a) = b     
     
type instance StrokeTypeFromLayer TLayerBBox = StrokeBBox

data TLayerSelectInPage s a = TLayerSelectInPage 
                              { gselectedlayer :: TLayerSelect a 
                              , gotherlayers :: s a
                              }


data TLayerSelectInPageBuf s a = TLayerSelectInPageBuf
                               { gselectedlayerbuf :: TLayerSelectBuf a 
                               , gotherlayersbuf :: s a
                               }


-- -- | 
-- newtype LyBuf = LyBuf { mbuffer :: Maybe Surface } 

-- | 
type instance StrokeTypeFromLayer (TLayerBBoxBuf b) = StrokeBBox 

-- -- | 
-- type TPageBBoxMapPDFBuf = 
--   TPageBBoxMapBkgBuf BackgroundPDFDrawable LyBuf
  
-- -- |   
-- type THoodleBBoxMapPDFBuf = 
--   THoodleBBoxMapBkgBuf BackgroundPDFDrawable LyBuf
  
-- |
type TTempPageSelectPDFBuf = 
  GPage BackgroundPDFDrawable (TLayerSelectInPageBuf ZipperSelect) (TLayerBBoxBuf LyBuf)

-- | 
type TTempHoodleSelectPDFBuf = 
  GSelect (IntMap TPageBBoxMapPDFBuf) (Maybe (Int, TTempPageSelectPDFBuf))


{-
-- | deprecated 

type TTempPageSelect = GPage Background (TLayerSelectInPage []) TLayerBBox
                       

-- | deprecated 

type TTempHoodleSelect = GSelect (IntMap TPageBBoxMap) (Maybe (Int, TTempPageSelect))

-}


-- type TLayerSelect a = GLayer TEitherAlterHitted (StrokeTypeFromLayer a) 
