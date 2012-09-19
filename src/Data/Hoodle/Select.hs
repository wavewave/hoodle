{-# LANGUAGE TypeFamilies, TypeOperators, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Hoodle.Select 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Data.Hoodle.Select where

-- from other packages
import Control.Lens
import Data.ByteString 
-- 

-- | 
data GSelect a b = GSelect { gselect_ttl :: ByteString 
                           , gselect_all :: a 
                           , gselect_selected :: b
                           }


-- |
gselTitle :: Simple Lens (GSelect a b) ByteString
gselTitle = lens gselect_ttl (\f a -> f {gselect_ttl = a})

-- |
gselAll :: Simple Lens (GSelect a b) a 
gselAll = lens gselect_all (\f a -> f {gselect_all = a} )

-- |
gselSelected :: Simple Lens (GSelect a b) b
gselSelected = lens gselect_selected (\f a -> f {gselect_selected = a})


