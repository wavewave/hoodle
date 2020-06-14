-----------------------------------------------------------------------------

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
module Control.Monad.Trans.Crtn.EventHandler where

import Control.Concurrent.MVar
import Control.Monad.State
import Control.Monad.Trans.Crtn.Driver
import Control.Monad.Trans.Crtn.Event
import Control.Monad.Trans.Crtn.Logger
import Control.Monad.Trans.Except

-- |
eventHandler :: MVar (Maybe (Driver e IO ())) -> e -> IO ()
eventHandler evar ev = do
  mnext <- takeMVar evar
  case mnext of
    Nothing -> return ()
    Just drv -> do
      (r, drv') <- eaction drv
      putMVar evar (Just drv')
      case r of
        Left err -> scribe (show err)
        Right Nothing -> return ()
        Right (Just (ActionOrder act)) ->
          act (eventHandler evar) >>= eventHandler evar
  where
    eaction = runStateT (runExceptT $ fire ev)
