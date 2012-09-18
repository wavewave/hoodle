{-# LANGUAGE TypeFamilies, OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Hoodle.Map 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Data.Hoodle.Map where

import Data.IntMap 
-- from this package
import Data.Hoodle.BBox
import Data.Hoodle.BBoxImg
import Data.Hoodle.Generic
import Data.Hoodle.Simple


type TPageMap = GPage Background IntMap TLayerSimple 

type THoodleMap = GHoodle [] TPageMap 

type TPageBBoxMap = GPage Background IntMap TLayerBBox

type THoodleBBoxMap = GHoodle IntMap TPageBBoxMap

type TPageBBoxMapBkg b = GPage b IntMap TLayerBBox

type THoodleBBoxMapBkg b = GHoodle IntMap (TPageBBoxMapBkg b)

-- | 
type TPageBBoxMapBkgImg b = GPage b IntMap TLayerBBoxImg

-- | 
type THoodleBBoxMapBkgImg b = GHoodle IntMap (TPageBBoxMapBkgImg b)


emptyGHoodleMap :: GHoodle IntMap a
emptyGHoodleMap = GHoodle "" empty

