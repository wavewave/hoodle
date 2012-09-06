-----------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.Trans.Crtn.EventHandler
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Common event handling 
-- 
-----------------------------------------------------------------------------

module Control.Monad.Trans.Crtn.EventHandler where 
  
import Control.Concurrent.MVar 
import Control.Monad.State 
import Control.Monad.Error
-- from this package 
-- import Control.Monad.Trans.Crtn
-- import Control.Monad.Trans.Crtn.Event 
import Control.Monad.Trans.Crtn.Driver 
import Control.Monad.Trans.Crtn.Logger 

-- | 
eventHandler :: MVar (Maybe (Driver e IO ())) -> e -> IO ()
eventHandler evar ev = do 
    mnext <- takeMVar evar
    case mnext of 
      Nothing -> return () 
      Just drv -> do                
        eaction drv >>= either (\err -> scribe (show err) >> return drv) return >>= putMVar evar . Just  
          where eaction = evalStateT $ runErrorT $ fire ev >> lift get >>= return
  
{-  
    drv <- takeMVar dref 
    eaction drv >>= either (\err -> scribe (show err) >> return drv) return >>= putMVar dref 
  where -- eaction :: Driver e IO () -> IO (Either (CrtnErr ()) (Driver e IO ()))
        eaction = evalStateT $ runErrorT $ fire ev >> lift get >>= return -}

