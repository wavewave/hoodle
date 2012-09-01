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
import Control.Monad.Trans.Free
import Data.IORef
-- from hoodle-platform
import Control.Monad.Trans.Crtn 
-- from this package 
import Hoodle.Type.Coroutine
import Hoodle.Type.Event 

-- | common event handler
bouncecallback :: TRef -> MyEvent -> IO () 
bouncecallback tref ev = do 
    mnext <- readIORef tref 
    case mnext of 
      Nothing -> do putStrLn "Nothing" 
                    print ev 
      Just next -> do                
        next' <- do 
          x <- runFreeT (next ev)
          case x of 
            Pure () -> error "end? in boundcallback" -- partial
            Free (Awt next') -> return next' 
        writeIORef tref (Just next')

