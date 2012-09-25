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

import Control.Applicative
import Control.Lens 
-- from this package
import Data.Hoodle.Generic
import Data.Hoodle.Simple

-- | 
type SLayer = GLayer () [] Item  -- Strokes 

-- | 
type SPage = GPage Background [] SLayer 

-- |
type SHoodle = GHoodle [] SPage 

-- | smart constructor for SLayer 
mkSLayer :: Layer -> SLayer
mkSLayer = GLayer () <$> view items -- strokes 

-- | smart constructor for SPage
mkSPage :: Page -> SPage 
mkSPage = GPage 
          <$> view dimension 
          <*> view background 
          <*> map mkSLayer . view layers  

-- | smart constructor for SHoodle 
mkSHoodle :: Hoodle -> SHoodle 
mkSHoodle = GHoodle 
            <$> view title 
            <*> map mkSPage . view pages

-- | 
slayer2Layer :: SLayer -> Layer 
slayer2Layer (GLayer _ itms) = Layer itms 

-- | 
spage2Page :: SPage -> Page 
spage2Page (GPage dim bkg lyrs) = Page dim bkg (map slayer2Layer lyrs)

-- | 
shoodle2Hoodle :: SHoodle -> Hoodle
shoodle2Hoodle (GHoodle ttl pgs) = Hoodle ttl (map spage2Page pgs)

