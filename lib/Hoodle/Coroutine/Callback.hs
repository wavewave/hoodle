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

-- from other packages
import Control.Concurrent 
import Control.Monad.Trans.Free
-- import Data.IORef
-- from hoodle-platform
import Control.Monad.Trans.Crtn 
-- from this package 
import Hoodle.Type.Coroutine
import Hoodle.Type.Event 

-- | common event handler
bouncecallback :: EventVar -> MyEvent -> IO () 
bouncecallback evar ev = do 
    mnext <- takeMVar evar
    case mnext of 
      Nothing -> return () 
      Just next -> do                
        enext' <- dispatch ev next
        either (error "end? in bouncecallback") (putMVar evar.Just) enext' 
        
{-        next' <- do 
          x <- runFreeT (next ev)
          case x of 
            Pure () -> error "end? in boundcallback" -- partial
            Free (Awt next') -> return next' 
        putMVar evar (Just next')
-}
