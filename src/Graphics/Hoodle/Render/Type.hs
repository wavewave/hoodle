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

import Data.IntMap 
import Graphics.Rendering.Cairo 
-- from hoodle-platform 
import Data.Hoodle.Generic
import Data.Hoodle.Simple
-- import Data.Hoodle.Simple
-- from this package
import Graphics.Hoodle.Render.Type.Background 
import Graphics.Hoodle.Render.Type.Item


-- | 
type RLayer = GLayer Surface [] RItem 

-- | 
type RPage = GPage RBackground IntMap RLayer 

-- |
type RHoodle = GHoodle IntMap RPage 


cnstrctRHoodle :: Hoodle -> IO RHoodle
cnstrctRHoodle = undefined 


