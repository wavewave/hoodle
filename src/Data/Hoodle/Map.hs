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
import Data.Hoodle.Simple
import Data.Hoodle.Generic
import Data.Hoodle.BBox

type TPageMap = GPage Background IntMap TLayerSimple 

type THoodleMap = GHoodle [] TPageMap 

type TPageBBoxMap = GPage Background IntMap TLayerBBox

type THoodleBBoxMap = GHoodle IntMap TPageBBoxMap

type TPageBBoxMapBkg b = GPage b IntMap TLayerBBox

type THoodleBBoxMapBkg b = GHoodle IntMap (TPageBBoxMapBkg b)


emptyGHoodleMap :: GHoodle IntMap a
emptyGHoodleMap = GHoodle "" empty

