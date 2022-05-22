{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | describe world object
module Sample where

import Control.Applicative
import Control.Category
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
--
import Control.Monad.Trans.Crtn
import Control.Monad.Trans.Crtn.Logger
import Control.Monad.Trans.Crtn.Object
import Control.Monad.Trans.Crtn.Queue
import Control.Monad.Trans.Crtn.World
import Control.Monad.Trans.Either
--
import Event
import SampleActor
--
import Prelude hiding (id, (.))

-- |
world :: forall m. (MonadIO m) => SObjT (WorldOp Event m) m ()
world = ReaderT staction
  where
    staction req = do
      runStateT (go req) initWorld
      return ()
    go ::
      (MonadIO m) =>
      Arg (WorldOp Event m) ->
      StateT (WorldAttrib (SObjBT (WorldOp Event m) m)) (SObjBT (WorldOp Event m) m) ()
    go (Arg GiveEvent ev) = do
      dobj <- (^. worldActor . objDoor) <$> get
      mobj <- (^. worldActor . objMessageBoard) <$> get
      aobj <- (^. worldActor . objAir) <$> get
      Right (dobj', mobj', aobj') <-
        runEitherT $ do
          d1 <- fst <$> EitherT (dobj <==| giveEventSub ev)
          m1 <- fst <$> EitherT (mobj <==| giveEventSub ev)
          a1 <- fst <$> EitherT (aobj <==| giveEventSub ev)
          return (d1, m1, a1)
      modify
        ( (worldActor . objDoor .~ dobj')
            . (worldActor . objMessageBoard .~ mobj')
            . (worldActor . objAir .~ aobj')
        )
      req <- lift (request (Res GiveEvent ()))
      go req
    go (Arg FlushLog (logobj :: LogServer m ())) = do
      logf <- (^. worldState . tempLog) <$> get
      let msg = logf ""
      if ((not . null) msg)
        then do
          Right (logobj', _) <- (lift . lift) (logobj <==| writeLog ("[World] " ++ (logf "")))
          modify (worldState . tempLog .~ id)
          req <- lift (request (Res FlushLog logobj'))
          go req
        else do
          req <- lift (request Ign)
          go req
    go (Arg FlushQueue ()) = do
      q <- (^. worldState . tempQueue) <$> get
      let lst = fqueue q ++ reverse (bqueue q)
      modify (worldState . tempQueue .~ emptyQueue)
      req <- lift (request (Res FlushQueue lst))
      go req
