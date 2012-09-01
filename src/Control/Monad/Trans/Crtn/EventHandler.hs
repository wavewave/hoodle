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
-- 
import Control.Monad.Trans.Crtn
import Control.Monad.Trans.Crtn.Event 
import Control.Monad.Trans.Crtn.Driver 
import Control.Monad.Trans.Crtn.Logger 

-- | 
eventHandler :: MVar (Driver IO ()) -> Event -> IO ()
eventHandler dref ev = do 
    drv <- takeMVar dref 
    eaction drv >>= either (\err -> scribe (show err) >> return drv) return >>= putMVar dref 
  where eaction :: Driver IO () -> IO (Either (CrtnErr ()) (Driver IO ()))
        eaction = evalStateT $ runErrorT $ fire ev >> lift get >>= return 

  