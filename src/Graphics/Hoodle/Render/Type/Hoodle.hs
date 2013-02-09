{-# LANGUAGE TypeFamilies, TypeOperators, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Type.Hoodle
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
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

import Control.Applicative
import Control.Lens 
import Data.Foldable (toList)
import qualified Data.IntMap as IM
import Graphics.Rendering.Cairo 
-- from hoodle-platform 
import Data.Hoodle.BBox
import Data.Hoodle.Generic
import Data.Hoodle.Simple
import Data.Hoodle.Zipper
-- from this package
import Graphics.Hoodle.Render.Type.Background 
import Graphics.Hoodle.Render.Type.Item

----------------------------
-- normal state rendering --
----------------------------

newtype LyBuf = LyBuf (Maybe Surface)

-- | normal rendering data structure for layer, R for rendering
--   buffer is Surface, container for item = list 
--   and StrokeBBox as contained items
type RLayer = GLayer LyBuf [] RItem -- StrokeBBox 

-- | normal rendering data structure for page 
--   background is RBackground and container for layer is IntMap 
--   and layer is RLayer  
type RPage = GPage RBackground ZipperSelect RLayer -- change from IntMap 

-- | normal rendering data struture for hoodle 
--   container for page is IntMap 
--   page is RPage
type RHoodle = GHoodle IM.IntMap RPage 

emptyRLayer :: RLayer 
emptyRLayer = GLayer (LyBuf Nothing) []


-------
-- get simple hoodle data structure 
-------

-- | project to simple Layer out of RLayer 
rLayer2Layer :: RLayer -> Layer
rLayer2Layer = Layer <$> fmap rItem2Item . view gitems 

 -- strkbbx_strk . view gstrokes

-- | project to simple Page out of RPage
rPage2Page :: RPage -> Page 
rPage2Page = Page <$> view gdimension 
                  <*> rbkg2Bkg . view gbackground 
                  <*> fmap rLayer2Layer . toList . view glayers

-- | project to simple Hoodle out of RHoodle 
rHoodle2Hoodle :: RHoodle -> Hoodle 
rHoodle2Hoodle = Hoodle <$> view ghoodleID
                        <*> view gtitle 
                        <*> view gembeddedpdf
                        <*> IM.elems . fmap rPage2Page . view gpages


----------------------      
----- Rendering   
----------------------

-- | 
newtype InBBox a = InBBox { unInBBox :: a }

-- | 
data InBBoxOption = InBBoxOption (Maybe BBox) 

-- |
newtype InBBoxBkgBuf a = InBBoxBkgBuf { unInBBoxBkgBuf :: a }


