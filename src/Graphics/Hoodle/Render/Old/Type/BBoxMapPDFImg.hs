{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Type.BBoxMapPDFImg
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Type.BBoxMapPDFImg where

import           Control.Lens
import           Data.IntMap
import           Graphics.Rendering.Cairo
-- from hoodle-platform 
import           Data.Hoodle.BBox
import           Data.Hoodle.BBoxImg
import           Data.Hoodle.Buffer
import           Data.Hoodle.Generic
import           Data.Hoodle.Map
import           Data.Hoodle.Select
import           Data.Hoodle.Simple
-- from this package 
import           Graphics.Hoodle.Render.Type.Select
import           Graphics.Hoodle.Render.PDFBackground

-- | 
type TPageBBoxMapPDFImg = TPageBBoxMapBkgImg BackgroundPDFDrawable 

-- | 
type THoodleBBoxMapPDFImg = THoodleBBoxMapBkgImg BackgroundPDFDrawable

-- | 
type TTempPageSelectPDFImg = GPage BackgroundPDFDrawable (TLayerSelectInPage []) TLayerBBoxImg

-- | 
type TTempHoodleSelectPDFImg = GSelect (IntMap TPageBBoxMapPDFImg) (Maybe (Int, TTempPageSelectPDFImg))

-- | 
newtype LyBuf = LyBuf { mbuffer :: Maybe Surface } 

-- | 
type instance StrokeTypeFromLayer (TLayerBBoxBufImg b) = StrokeBBoxImg

-- | 
type TPageBBoxMapPDFBufImg = 
  TPageBBoxMapBkgBufImg BackgroundPDFDrawable LyBuf
  
-- |   
type THoodleBBoxMapPDFBufImg = 
  THoodleBBoxMapBkgBufImg BackgroundPDFDrawable LyBuf
  
-- |
type TTempPageSelectPDFBufImg = 
  GPage BackgroundPDFDrawable (TLayerSelectInPageBuf ZipperSelect) (TLayerBBoxBufImg LyBuf)

-- | 
type TTempHoodleSelectPDFBufImg = 
  GSelect (IntMap TPageBBoxMapPDFBufImg) (Maybe (Int, TTempPageSelectPDFBufImg))

-- | 
instance GCast (TLayerBBoxImg)  (TLayerBBoxBufImg LyBuf) where
  gcast lyr = GLayerBuf (LyBuf Nothing) (view g_strokes $ lyr) 

-- | 
instance GCast Layer (TLayerBBoxBufImg LyBuf) where
  gcast lyr = gcast (fmap gcast (fromLayer lyr :: TLayerBBox) :: TLayerBBoxImg)
