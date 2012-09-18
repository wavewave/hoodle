{-# LANGUAGE TypeFamilies, TypeOperators, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Hoodle.Generic.Simple 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Simple Hoodle Type in Generic parameterized type
-- 
-----------------------------------------------------------------------------

module Data.Hoodle.Generic.Simple where

-- from this package
import Data.Hoodle.Generic
import Data.Hoodle.Simple

-- | 
type SLayer = GLayer () [] Item 

-- | 
type SPage = GPage Background [] SLayer 

-- |
type SHoodle = GHoodle [] SPage 
