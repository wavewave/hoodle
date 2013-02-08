-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.ModelAction.Item
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.ModelAction.Item where

-- from other packages
import           Control.Category
import           Control.Lens (view,set,over)
import           Data.IORef
import           Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk as Gtk (get)
-- from hoodle-platform 
import           Data.Hoodle.Generic
import           Data.Hoodle.Zipper
import           Graphics.Hoodle.Render.Type
-- 
import           Hoodle.Util
import           Hoodle.Type.Alias
-- 
import Prelude hiding ((.),id)


