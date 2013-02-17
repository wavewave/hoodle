{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Clipboard 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Type.Clipboard where

import           Control.Category
import           Control.Lens
-- from hoodle-platform
import           Data.Hoodle.BBox
import           Data.Hoodle.Simple
--
import Prelude hiding ((.), id)

-- |
newtype Clipboard = Clipboard { unClipboard :: [BBoxed Stroke] }

-- |
emptyClipboard :: Clipboard
emptyClipboard = Clipboard []

-- |
isEmpty :: Clipboard -> Bool 
isEmpty = null . unClipboard 

-- |
getClipContents :: Clipboard -> [BBoxed Stroke] 
getClipContents = unClipboard

-- |
replaceClipContents :: [BBoxed Stroke] -> Clipboard -> Clipboard
replaceClipContents strs _ = Clipboard strs 


-- makeLenses ''SelectInfo
