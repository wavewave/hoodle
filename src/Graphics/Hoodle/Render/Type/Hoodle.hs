{-# LANGUAGE TypeFamilies, TypeOperators, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Type.Hoodle
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

module Graphics.Hoodle.Render.Type.Hoodle where


import Data.IntMap 
import Graphics.Rendering.Cairo 
-- from hoodle-platform 
import Data.Hoodle.BBox
import Data.Hoodle.Generic
import Data.Hoodle.Simple
import Data.Hoodle.Zipper
-- from this package
import Graphics.Hoodle.Render.Type.Background 
-- import Graphics.Hoodle.Render.Type.Item

----------------------------
-- normal state rendering --
----------------------------

newtype LyBuf = LyBuf (Maybe Surface)

-- | normal rendering data structure for layer, R for rendering
--   buffer is Surface, container for item = list 
--   and StrokeBBox as contained items
type RLayer = GLayer LyBuf [] StrokeBBox -- RItem 

-- | normal rendering data structure for page 
--   background is RBackground and container for layer is IntMap 
--   and layer is RLayer  
type RPage = GPage RBackground ZipperSelect RLayer -- change from IntMap 

-- | normal rendering data struture for hoodle 
--   container for page is IntMap 
--   page is RPage
type RHoodle = GHoodle IntMap RPage 

emptyRLayer :: RLayer 
emptyRLayer = GLayer (LyBuf Nothing) []

{-

cnstrctRHoodle :: Hoodle -> IO RHoodle
cnstrctRHoodle = undefined 

-}