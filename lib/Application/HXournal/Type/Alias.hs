{-# LANGUAGE EmptyDataDecls, TypeFamilies, RankNTypes #-}

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

import Data.Xournal.Generic
import Data.Xournal.Buffer
import Data.Xournal.Select
import Graphics.Xournal.Render.Type.Select
import Graphics.Xournal.Render.BBoxMapPDF
import Graphics.Xournal.Render.PDFBackground

data EditMode = EditMode 
data SelectMode = SelectMode 

type family Xournal a :: *
-- type family Page a :: * 
type family Layer a :: * 
     
class GPageable a where  
  type AssocBkg a :: *
  type AssocStream a :: * -> *
  type AssocLayer a :: *
       
       
type Page a = GPage (AssocBkg a) (AssocStream a) (AssocLayer a)

instance GPageable EditMode where
  type AssocBkg EditMode = BackgroundPDFDrawable
  type AssocStream EditMode = ZipperSelect
  type AssocLayer EditMode = TLayerBBoxBuf LyBuf 
  
instance GPageable SelectMode where
  type AssocBkg SelectMode = BackgroundPDFDrawable
  type AssocStream SelectMode = TLayerSelectInPageBuf ZipperSelect
  type AssocLayer SelectMode = TLayerBBoxBuf LyBuf
  
  
-- type instance Page EditMode = TPageBBoxMapPDFBuf
-- type instance Page SelectMode = TTempPageSelectPDFBuf 


type instance Layer EditMode = TLayerBBoxBuf LyBuf
type instance Layer SelectMode = TLayerSelectInPageBuf ZipperSelect (TLayerBBoxBuf LyBuf)

type instance Xournal EditMode = TXournalBBoxMapPDFBuf 
type instance Xournal SelectMode = TTempXournalSelectPDFBuf 

