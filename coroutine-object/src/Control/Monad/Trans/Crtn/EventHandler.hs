module Control.Monad.Trans.Crtn.EventHandler where

import Control.Concurrent.MVar
  ( MVar,
    putMVar,
    takeMVar,
  )
import Control.Monad.State (runStateT)
import Control.Monad.Trans.Crtn.Driver (Driver, fire)
import Control.Monad.Trans.Crtn.Event (ActionOrder (..))
import Control.Monad.Trans.Crtn.Logger (scribe)
import Control.Monad.Trans.Except (runExceptT)

import Debug.Trace (traceEventIO)

-- |
eventHandler :: MVar (Maybe (Driver e IO ())) -> e -> IO ()
eventHandler evar ev = do
  traceEventIO "eventHandler is called"
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
