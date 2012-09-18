{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Hoodle.Buffer 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Data.Hoodle.Buffer where

import Data.IntMap 
-- from this package
import Data.Hoodle.BBox
import Data.Hoodle.BBoxImg
import Data.Hoodle.Generic
import Data.Hoodle.Select

-- |
type TLayerBBoxBuf buf = GLayerBuf buf [] StrokeBBox

-- |
type TPageBBoxMapBkgBuf bkg buf = GPage bkg ZipperSelect (TLayerBBoxBuf buf)

-- |
type THoodleBBoxMapBkgBuf bkg buf = 
  GHoodle IntMap (TPageBBoxMapBkgBuf bkg buf)
  
-- |
type TLayerBBoxBufImg buf = GLayerBuf buf [] StrokeBBoxImg

-- | 
type TPageBBoxMapBkgBufImg bkg buf = GPage bkg ZipperSelect (TLayerBBoxBufImg buf)

-- | 
type THoodleBBoxMapBkgBufImg bkg buf = 
  GHoodle IntMap (TPageBBoxMapBkgBufImg bkg buf)
  

