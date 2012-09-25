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

-- from hoodle-platform
import Graphics.Hoodle.Render.Type

-- | 
data EditMode = EditMode 

-- | 
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


