{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Callback 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Callback where

-- from hoodle-platform
import Control.Monad.Trans.Crtn.EventHandler 
-- from this package 
import Hoodle.Type.Coroutine
import Hoodle.Type.Event 

-- | common event handler
bouncecallback :: EventVar -> MyEvent -> IO () 
bouncecallback = eventHandler
  
