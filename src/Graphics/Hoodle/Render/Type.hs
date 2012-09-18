{-# LANGUAGE TypeFamilies, TypeOperators, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Type 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Renderable Hoodle Type  
-- 
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Type where

-- from this package
import Data.Hoodle.Generic
import Data.Hoodle.Simple

-- | 
type RNLayer = GLayer () [] Stroke 

-- | 
type RNPage = GPage Background [] SLayer 

-- |
type RNHoodle = GHoodle [] SPage 




