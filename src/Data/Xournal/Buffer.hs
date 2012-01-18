{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Xournal.Buffer 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Data.Xournal.Buffer where

import Data.IntMap 
import Data.Xournal.Select
import Data.Xournal.Simple
import Data.Xournal.Generic
import Data.Xournal.BBox
import Data.Xournal.Map

type TLayerBBoxBuf buf = GLayerBuf buf [] StrokeBBox

type TPageBBoxMapBkgBuf bkg buf = GPage bkg ZipperSelect (TLayerBBoxBuf buf)

type TXournalBBoxMapBkgBuf bkg buf = 
  GXournal IntMap (TPageBBoxMapBkgBuf bkg buf)
  
