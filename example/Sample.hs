{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables, KindSignatures #-}

----------------------------
-- | describe world object
----------------------------

module Sample where 

import Control.Applicative
import Control.Category
import Control.Monad.Error 
import Control.Monad.Reader
import Control.Monad.State
-- import Data.Lens.Common 
import Control.Lens
-- 
import Control.Monad.Trans.Crtn
import Control.Monad.Trans.Crtn.Logger 
import Control.Monad.Trans.Crtn.Object
import Control.Monad.Trans.Crtn.Queue
import Control.Monad.Trans.Crtn.World
-- 
import Event
import SampleActor

-- 
import Prelude hiding ((.),id)


-- | 
world :: forall m. (MonadIO m) => SObjT (WorldOp Event m) m () 
world = ReaderT staction 
  where 
    staction req = do runStateT (go req) initWorld
                      return ()
    go :: (MonadIO m) => 
          Arg (WorldOp Event m) 
          -> StateT (WorldAttrib (SObjBT (WorldOp Event m) m)) (SObjBT (WorldOp Event m) m) () 
    go (Arg GiveEvent ev) = do
      dobj <- (^. worldActor.objDoor) <$> get  
      mobj <- (^. worldActor.objMessageBoard) <$> get 
      aobj <- (^. worldActor.objAir) <$> get 
      Right (dobj',mobj',aobj') <- 
        runErrorT $ (,,) <$> liftM fst (dobj <==| giveEventSub ev) 
                         <*> liftM fst (mobj <==| giveEventSub ev) 
                         <*> liftM fst (aobj <==| giveEventSub ev)
      modify (  (worldActor.objDoor .~ dobj') 
              . (worldActor.objMessageBoard .~ mobj')
              . (worldActor.objAir .~ aobj') )
      req <- lift (request (Res GiveEvent ()))
      go req   
    go (Arg FlushLog (logobj :: LogServer m ())) = do
      logf <- (^. worldState.tempLog) <$> get 
      let msg = logf "" 
      if ((not . null) msg) 
        then do 
          let l1 = runErrorT (logobj <==| writeLog ("[World] " ++ (logf ""))) 
          Right (logobj',_) <- lift . lift $ l1
          modify (worldState.tempLog .~ id)
          req <- lift (request (Res FlushLog logobj'))
          go req  
        else do 
          req <- lift (request Ign) 
          go req 
    go (Arg FlushQueue ()) = do
      q <- (^. worldState.tempQueue) <$> get 
      let lst = fqueue q ++ reverse (bqueue q)
      modify (worldState.tempQueue .~ emptyQueue)
      req <- lift (request (Res FlushQueue lst))
      go req 
      
      