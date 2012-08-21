{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables, KindSignatures #-}

----------------------------
-- | describe world object
----------------------------

module Control.Monad.Coroutine.World.Sample2 where 

import Control.Applicative
import Control.Category
import Control.Monad.Error 
import Control.Monad.Reader
import Control.Monad.State
-- import Data.Lens.Common 
import Control.Lens
-- 
import Control.Monad.Coroutine
import Control.Monad.Coroutine.Logger 
import Control.Monad.Coroutine.Object
import Control.Monad.Coroutine.Queue
import Control.Monad.Coroutine.World
import Control.Monad.Coroutine.World.SampleActor2

-- 
import Prelude hiding ((.),id)


-- | 
world :: forall m. (MonadIO m) => ServerObj (WorldOp m) m () 
world = ReaderT staction 
  where 
    staction req = do runStateT (go req) initWorld
                      return ()
    go :: (MonadIO m) => MethodInput (WorldOp m) -> StateT (WorldAttrib (ServerT (WorldOp m) m)) (ServerT (WorldOp m) m) () 
    go (Input GiveEvent ev) = do
      wobj <- get >>= return . (^. worldActor.objWorker )  
      Right wobj' <- 
        runErrorT $ liftM fst (wobj <==> giveEventSub ev) 
      modify (worldActor.objWorker .~ wobj')
      -- put . (worldActor.objWorker .~ wobj')  =<< get       
      req <- lift (request (Output GiveEvent ()))
      go req   
    go (Input FlushLog (logobj :: LogServer m ())) = do
      logf <- get >>= return . (^. worldState.tempLog )
      let msg = logf "" 
      if ((not . null) msg) 
        then do 
          let l1 = runErrorT (logobj <==> writeLog ("[World] " ++ (logf ""))) 
          Right (logobj',_) <- lift . lift $ l1
          modify (worldState.tempLog .~ id)
          -- put . (worldState.tempLog .~ id) =<< get
          req <- lift (request (Output FlushLog logobj'))
          go req  
        else do 
          req <- lift (request Ignore) 
          go req 
    go (Input FlushQueue ()) = do
      q <- ( ^. worldState.tempQueue ) <$> get
      let lst = fqueue q ++ reverse (bqueue q)
      modify ( worldState.tempQueue .~ emptyQueue )
      -- put =<< ( worldState.tempQueue .~ emptyQueue ) <$> get 
      req <- lift (request (Output FlushQueue lst))
      go req 