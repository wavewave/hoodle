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

import Control.Monad.Coroutine 
import Control.Monad.State
import Control.Monad.Trans.Free
-- import Control.Monad.Coroutine.SuspensionFunctors
import Data.IORef
import Hoodle.Type.Coroutine
import Hoodle.Type.XournalState
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
            Free (Await next') -> return next' 
        writeIORef tref (Just next')

