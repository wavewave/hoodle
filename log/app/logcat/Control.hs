{-# LANGUAGE DeriveFunctor #-}

module Control
  ( ControlF (..),
    Control,
    getState,
    putState,
    nextEvent,
    stepControl,
  )
where

import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Types (LogcatState)

data ControlF r
  = GetState (LogcatState -> r)
  | PutState LogcatState r
  | NextEvent r
  deriving (Functor)

type Control = Free ControlF

getState :: Control LogcatState
getState = liftF (GetState id)

putState :: LogcatState -> Control ()
putState s = liftF (PutState s ())

nextEvent :: Control ()
nextEvent = liftF (NextEvent ())

stepControl :: Control r -> ReaderT (TVar LogcatState) IO (Either (Control r) r)
stepControl (Pure r) = pure (Right r)
stepControl (Free (GetState cont)) = do
  liftIO $ putStrLn "getState"
  ref <- ask
  s <- liftIO $ atomically $ readTVar ref
  pure (Left (cont s))
stepControl (Free (PutState s next)) = do
  liftIO $ putStrLn "putState"
  ref <- ask
  liftIO $ atomically $ writeTVar ref s
  pure (Left next)
stepControl (Free (NextEvent next)) = do
  liftIO $ putStrLn "nextEvent"
  pure (Left next)
