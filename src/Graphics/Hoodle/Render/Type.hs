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
import Data.Hoodle.BBox
import Data.Hoodle.Generic
import Data.Hoodle.Simple
-- from this package
import Graphics.Hoodle.Render.Type.Background 
-- import Graphics.Hoodle.Render.Type.Item

----------------------------
-- normal state rendering --
----------------------------

-- | normal rendering data structure for layer, R for rendering
--   buffer is Surface, container for item = list 
--   and StrokeBBox as contained items
type RLayer = GLayer Surface [] StrokeBBox -- RItem 

-- | normal rendering data structure for page 
--   background is RBackground and container for layer is IntMap 
--   and layer is RLayer  
type RPage = GPage RBackground IntMap RLayer 

-- | normal rendering data struture for hoodle 
--   container for page is IntMap 
--   page is RPage
type RHoodle = GHoodle IntMap RPage 


{-
----------------------------
-- select state rendering --
----------------------------

-- | select rendering data structure for layer, H for highlight 
--   buffer is surface, container is TEitherAlterHitted 
--   items are StrokeBBox
type HLayer = GLayer Surface TEitherAlterHitted StrokeBBox
-}



cnstrctRHoodle :: Hoodle -> IO RHoodle
cnstrctRHoodle = undefined 


