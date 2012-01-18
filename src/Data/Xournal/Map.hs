{-# LANGUAGE TypeFamilies, OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Xournal.Map 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Data.Xournal.Map where

import Data.IntMap 
import Data.Xournal.Simple
import Data.Xournal.Generic
import Data.Xournal.BBox

type TPageMap = GPage Background IntMap TLayerSimple 

type TXournalMap = GXournal [] TPageMap 

type TPageBBoxMap = GPage Background IntMap TLayerBBox

type TXournalBBoxMap = GXournal IntMap TPageBBoxMap

type TPageBBoxMapBkg b = GPage b IntMap TLayerBBox

type TXournalBBoxMapBkg b = GXournal IntMap (TPageBBoxMapBkg b)


emptyGXournalMap :: GXournal IntMap a
emptyGXournalMap = GXournal "" empty

