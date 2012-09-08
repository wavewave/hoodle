{-# LANGUAGE EmptyDataDecls, TypeFamilies, RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Alias
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
----------------------------------------------------------------------------

module Hoodle.Type.Alias where

import Data.Hoodle.Generic
import Data.Hoodle.Buffer
import Data.Hoodle.Select
import Graphics.Hoodle.Render.Type.Select
import Graphics.Hoodle.Render.BBoxMapPDF
import Graphics.Hoodle.Render.PDFBackground

data EditMode = EditMode 
data SelectMode = SelectMode 

type family Hoodle a :: *
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

type instance Hoodle EditMode = THoodleBBoxMapPDFBuf 
type instance Hoodle SelectMode = TTempHoodleSelectPDFBuf 

