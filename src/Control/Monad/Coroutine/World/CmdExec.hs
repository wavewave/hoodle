{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables, KindSignatures #-}

----------------------------
-- | describe world object
----------------------------

module Control.Monad.Coroutine.World.CmdExec where 

import Control.Applicative
import Control.Category
import Control.Concurrent
import Control.Lens
import Control.Monad.Error 
import Control.Monad.Reader
import Control.Monad.State
-- from this package 
import Control.Monad.Coroutine
import Control.Monad.Coroutine.Event 
import Control.Monad.Coroutine.Logger 
import Control.Monad.Coroutine.Object
import Control.Monad.Coroutine.Queue
import Control.Monad.Coroutine.World
-- 
import Prelude hiding ((.),id)

-- | 

data JobStatus = None | Started | Ended 
               deriving (Show,Eq)

-- | 
data SubOp i o where 
  GiveEventSub :: SubOp Event ()


-- | full state of world 
data WorldState = WorldState { _jobStatus :: JobStatus
                             , _bufLog :: String -> String 
                             , _bufQueue :: Queue (Either ActionOrder Event)
                             }


-- | isDoorOpen lens
jobStatus :: Simple Lens WorldState JobStatus
jobStatus = lens _jobStatus (\a b -> a { _jobStatus = b })

-- | 
bufLog :: Simple Lens WorldState (String -> String) 
bufLog = lens _bufLog (\a b -> a { _bufLog = b } )

-- | 
bufQueue :: Simple Lens WorldState (Queue (Either ActionOrder Event))
bufQueue = lens _bufQueue (\a b -> a { _bufQueue = b} )


-- | 
emptyWorldState :: WorldState 
emptyWorldState = WorldState None id emptyQueue 


type WorldObject m r = ServerObj SubOp (StateT (WorldAttrib m) m) r 

-- | full collection of actors in world 
data WorldActor m 
    = WorldActor { _objWorker :: WorldObject m () 
                 } 


-- | objWorker lens
objWorker :: Simple Lens (WorldActor m) (ServerObj SubOp (StateT (WorldAttrib m) m) ())
objWorker = lens _objWorker (\a b -> a { _objWorker = b })


-- | 
initWorldActor :: (Monad m) => WorldActor m 
initWorldActor = WorldActor { _objWorker = worker
                            }

-- | 
data WorldAttrib m =
       WorldAttrib { _worldState :: WorldState
                   , _worldActor :: WorldActor m } 



-- | lens
worldState :: Simple Lens (WorldAttrib m) WorldState
worldState = lens _worldState (\a b -> a {_worldState = b})

-- | lens 
worldActor :: Simple Lens (WorldAttrib m) (WorldActor m)
worldActor = lens _worldActor (\a b -> a {_worldActor = b})



-- | initialization   
initWorld :: (Monad m) => WorldAttrib m 
initWorld = WorldAttrib emptyWorldState initWorldActor





-- |
giveEventSub :: (Monad m) => Event -> ClientObj SubOp m () 
giveEventSub ev = request (Input GiveEventSub ev) >> return ()



-- |
worker :: (Monad m) => ServerObj SubOp (StateT (WorldAttrib m) m) ()
worker = ReaderT workerW 
  where workerW (Input GiveEventSub ev) = do 
          r <- case ev of 
                 Start -> do 
                   let action = Left . ActionOrder $ 
                         \evhandler -> do 
                           forkIO $ do threadDelay 10000000
                                       putStrLn "BAAAAAMM"
                                       evhandler Finished
                           return ()
                   modify (worldState.bufQueue %~ enqueue action)
                   modify (worldState.jobStatus .~ Started)
                   return True
                 Finished -> do 
                   modify (worldState.jobStatus .~ Ended)
                   return True 
                 Render -> do 
                   st <- (^. worldState.jobStatus) <$> get 
                   modify (worldState.bufLog %~ (. (++ "job status = " ++ show st ++ "\n")))
                   return True
                 _ -> return False 
          req <- if r then request (Output GiveEventSub ())
                      else request Ignore 
          workerW req 
          
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
      req <- lift (request (Output GiveEvent ()))
      go req   
    go (Input FlushLog (logobj :: LogServer m ())) = do
      logf <- get >>= return . (^. worldState.bufLog )
      let msg = logf "" 
      if ((not . null) msg) 
        then do 
          let l1 = runErrorT (logobj <==> writeLog ("[World] " ++ (logf ""))) 
          Right (logobj',_) <- lift . lift $ l1
          modify (worldState.bufLog .~ id)
          req <- lift (request (Output FlushLog logobj'))
          go req  
        else do 
          req <- lift (request Ignore) 
          go req 
    go (Input FlushQueue ()) = do
      q <- ( ^. worldState.bufQueue ) <$> get
      let lst = fqueue q ++ reverse (bqueue q)
      modify ( worldState.bufQueue .~ emptyQueue )
      req <- lift (request (Output FlushQueue lst))
      go req 