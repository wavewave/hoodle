{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE ExistentialQuantification, OverloadedStrings, 
--              FlexibleInstances, FlexibleContexts,  
--              TypeFamilies, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Simple
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Simplest rendering of hoodle
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Simple where

import           Control.Applicative 
import           Control.Lens
import           Control.Monad.State hiding (mapM_)
-- import           Data.ByteString hiding (putStrLn)
import qualified Data.ByteString as S
import           Data.Foldable 
import qualified Data.Map as M
import           Data.Monoid 
import           Graphics.Rendering.Cairo
--
-- from hoodle-platform
import Data.Hoodle.BBox
import Data.Hoodle.Predefined 
import Data.Hoodle.Simple
-- from this package
import Graphics.Hoodle.Render.Background 
import Graphics.Hoodle.Render.Util.Draw 
-- import Graphics.Hoodle.Render.Generic
--
import Prelude hiding (mapM_)






