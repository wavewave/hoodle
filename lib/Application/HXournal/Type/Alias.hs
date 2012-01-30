{-# LANGUAGE EmptyDataDecls, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Type.Alias
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
----------------------------------------------------------------------------

module Application.HXournal.Type.Alias where

import Data.Xournal.Buffer
import Data.Xournal.Select
import Graphics.Xournal.Render.Type.Select
import Graphics.Xournal.Render.BBoxMapPDF

data EditMode
data SelectMode

type family Xournal a :: *
type family Page a :: * 
type family Layer a :: * 
     
type instance Page EditMode = TPageBBoxMapPDFBuf
type instance Page SelectMode = TTempPageSelectPDFBuf 

type instance Layer EditMode = TLayerBBoxBuf LyBuf
type instance Layer SelectMode = TLayerSelectInPageBuf ZipperSelect (TLayerBBoxBuf LyBuf)

type instance Xournal EditMode = TXournalBBoxMapPDFBuf 
type instance Xournal SelectMode = TTempXournalSelectPDFBuf 