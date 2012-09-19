{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Highlight
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- collection of rendering routine that draws bounding box only
--
-----------------------------------------------------------------------------


module Graphics.Hoodle.Render.Highlight where

import           Control.Lens 
import           Data.Foldable
import qualified Data.Map as M
import           Graphics.Rendering.Cairo
-- from hoodle-platform 
import Data.Hoodle.Generic
import Data.Hoodle.Simple
import Data.Hoodle.BBox
import Data.Hoodle.Predefined 
-- from this package
import Graphics.Hoodle.Render.Background 
import Graphics.Hoodle.Render.Type 
import Graphics.Hoodle.Render.Type.Background
-- import Graphics.Hoodle.Render.Type.Item
-- 
import Prelude hiding (fst,snd,curry,uncurry,mapM_,concatMap)
