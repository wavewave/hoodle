{-# LANGUAGE DeriveFunctor #-}

module Control
  ( ControlF (..),
    Control,
    tick,
    stepControl,
  )
where

import Control.Monad.Free (Free (..), liftF)

data ControlF r
  = Tick r
  deriving (Functor)

type Control = Free ControlF

tick :: Control ()
tick = liftF (Tick ())

stepControl :: Control r -> IO (Either (Control r) r)
stepControl (Pure r) = pure (Right r)
stepControl (Free (Tick next)) = do
  putStrLn "tick"
  pure (Left next)
