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

module Data.Hoodle.Buffer where

import Data.IntMap 
import Data.Hoodle.Select
-- import Data.Hoodle.Simple
import Data.Hoodle.Generic
import Data.Hoodle.BBox
-- import Data.Hoodle.Map

type TLayerBBoxBuf buf = GLayerBuf buf [] StrokeBBox

type TPageBBoxMapBkgBuf bkg buf = GPage bkg ZipperSelect (TLayerBBoxBuf buf)

type THoodleBBoxMapBkgBuf bkg buf = 
  GHoodle IntMap (TPageBBoxMapBkgBuf bkg buf)
  

