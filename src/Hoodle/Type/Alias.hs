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
-- import Data.Hoodle.Buffer
import Data.Hoodle.Select
import Data.Hoodle.Zipper 
-- import Graphics.Hoodle.Render.Type.Select
import Graphics.Hoodle.Render
import Graphics.Hoodle.Render.Background
import Graphics.Hoodle.Render.Type
import Graphics.Hoodle.Render.Type.Background 
import Graphics.Hoodle.Render.Type.Select

data EditMode = EditMode 
data SelectMode = SelectMode 

type family Hoodle a :: *
type family Page a :: * 
type family Layer a :: * 
     


-- type instance Layer EditMode = RLayer 
-- type instance Layer SelectMode = HLayers

type instance Page EditMode = RPage
type instance Page SelectMode = HPage 


type instance Hoodle EditMode = RHoodle
type instance Hoodle SelectMode = HHoodle 



{-
class GPageable a where  
  type AssocBkg a :: *
  type AssocContainer a :: * -> *
  type AssocLayer a :: *
       
       
type Page a = GPage (AssocBkg a) (AssocContainer a) (AssocLayer a)

instance GPageable EditMode where
  type AssocBkg EditMode = RBackground
  type AssocContainer EditMode = IntMap -- ZipperSelect
  type AssocLayer EditMode = RLayer 
  
instance GPageable SelectMode where
  type AssocBkg SelectMode = RBackground
  type AssocContainer SelectMode = TLayerSelectInPageBuf ZipperSelect
  type AssocLayer SelectMode = RLayer
-}  
  
-- type instance Page EditMode = TPageBBoxMapPDFBuf
-- type instance Page SelectMode = TTempPageSelectPDFBuf 


