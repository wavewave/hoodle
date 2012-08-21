{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables, KindSignatures #-}

----------------------------
-- | describe world object
----------------------------

module Control.Monad.Coroutine.World.CmdExec where 

import Control.Applicative hiding (empty)
import Control.Category
import Control.Concurrent
import Control.Lens
import Control.Monad.Error 
import Control.Monad.Reader
import Control.Monad.State
import Data.Map hiding (null)
import Data.Map.Lens 
import Data.Monoid
import Data.SafeCopy
import Data.Serialize.Get 
import Data.Serialize.Put
import Data.UUID hiding (null)
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
data SubOp i o where 
  GiveEventSub :: SubOp CmdExecEvent ()

-- | 
data CmdExecEvent = Start 
                  | Init Int 
                  | Finish Int
                  | Render 
                    deriving (Show,Eq)


-- | 
instance SafeCopy CmdExecEvent where
  putCopy Start = contain (safePut (1 :: Int))
  putCopy (Init n) = contain $ safePut (2 :: Int) >> safePut n 
  putCopy (Finish n) = contain $ safePut (3 :: Int) >> safePut n 
  putCopy Render = contain $ safePut (4 :: Int)
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of 
                           1 -> return Start 
                           2 -> Init <$> safeGet
                           3 -> Finish <$> safeGet
                           4 -> pure Render 
                           _ -> error "failed in getCopy of CmdExecEvent"


evuuid :: UUID
evuuid = case (fromString "858066b5-e6e7-431d-9ff3-facc5eb0befc") of 
           Nothing -> error "evuuid in CmdExec.hs" 
           Just i -> i 

evwrap :: CmdExecEvent -> Event 
evwrap ev = Event (evuuid,runPut (safePut ev))


-- | 
instance Eventable CmdExecEvent where
  eventClassID _ = evuuid 
  eventWrap = evwrap 

getCmdExecEvent :: Event -> Maybe CmdExecEvent 
getCmdExecEvent (Event (i,bstr))  
    | i == evuuid = either (const Nothing) Just (runGet safeGet bstr)
    | otherwise = Nothing 

-- |
giveEventSub :: (Monad m) => CmdExecEvent -> ClientObj SubOp m () 
giveEventSub ev = request (Input GiveEventSub ev) >> return ()


-- | 

data JobStatus = None | Started | Ended 
               deriving (Show,Eq)



-- | full state of world 
data WorldState = WorldState { _nextID :: Int 
                             , _jobMap :: Map Int JobStatus
                             , _bufLog :: String -> String 
                             , _bufQueue :: Queue (Either ActionOrder Event) }

-- | lens 
nextID :: Simple Lens WorldState Int 
nextID = lens _nextID (\a b -> a { _nextID = b })


-- | lens
jobMap :: Simple Lens WorldState (Map Int JobStatus)
jobMap = lens _jobMap (\a b -> a { _jobMap = b })


-- | 
bufLog :: Simple Lens WorldState (String -> String) 
bufLog = lens _bufLog (\a b -> a { _bufLog = b } )


-- | 
bufQueue :: Simple Lens WorldState (Queue (Either ActionOrder Event))
bufQueue = lens _bufQueue (\a b -> a { _bufQueue = b} )


-- | 
emptyWorldState :: WorldState 
emptyWorldState = WorldState { _nextID = 0 
                             , _jobMap = empty 
                             , _bufLog = id 
                             , _bufQueue = emptyQueue } 


-- | type synonym for actor object in world 
type WorldObject m r = ServerObj SubOp (StateT (WorldAttrib m) m) r 



-- | full collection of actors in world 
data WorldActor m 
    = WorldActor { _workers :: [WorldObject m ()] } 


-- | objWorker lens
workers :: Simple Lens (WorldActor m) [WorldObject m ()]
workers = lens _workers (\a b -> a { _workers = b })


-- | 
initWorldActor :: (Monad m) => WorldActor m 
initWorldActor = WorldActor { _workers = [] }

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


-- | command executor actor
cmdexec :: forall m. (Monad m) => 
           Int        -- ^ id 
        -> ServerObj SubOp (StateT (WorldAttrib m) m) ()
cmdexec idnum = ReaderT (workerW idnum None)  
  where 
    workerW :: Int 
            -> JobStatus 
            -> MethodInput SubOp 
            -> ServerT SubOp (StateT (WorldAttrib m) m) ()
    workerW i jst (Input GiveEventSub ev) = do 
      (r,jst') <- case ev of 
          Init i' -> do 
            if i == i' 
              then do 
                let action = Left . ActionOrder $ 
                      \evhandler -> do 
                        forkIO $ do threadDelay 10000000
                                    putStrLn "BAAAAAMM"
                                    evhandler (eventWrap (Finish i))
--  $ Event (evuuid,runPut (safePut (Finish i)))
                        return ()
                modify (worldState.bufQueue %~ enqueue action)
                return (True,Started)
              else return (False,jst)
          Finish i' -> do
            if i == i' then return (True,Ended) 
                       else return (False,jst)
          Render -> do 
            modify (worldState.bufLog %~ 
                       (. (<> show i <> "th job status = " <> show jst <> "\n")))
            return (True,jst)
          _ -> return (False,jst)

      modify (worldState.jobMap.at i .~ Just jst')
      req <- if r then request (Output GiveEventSub ())
                  else request Ignore 
      workerW i jst' req 

          
-- | 
world :: forall m. (MonadIO m) => ServerObj (WorldOp m) m () 
world = ReaderT staction 
  where 
    staction req = do runStateT (go req) initWorld
                      return ()
    go :: (MonadIO m) => MethodInput (WorldOp m) -> StateT (WorldAttrib (ServerT (WorldOp m) m)) (ServerT (WorldOp m) m) () 
    go (Input GiveEvent ev) = do
      case getCmdExecEvent ev of  
        Nothing -> return () 
        Just e -> do wlst <- (^. worldActor.workers ) <$> get 
                     case e of 
                       Start -> do 
                         i <- (^. worldState.nextID) <$> get 
                         let wlst' = cmdexec i : wlst 
                         modify (worldActor.workers .~ wlst')
                         modify (worldState.nextID %~ (+1) )
                       _ -> do  
                         Right wlst' <- 
                           runErrorT $ mapM (\x -> liftM fst (x <==> giveEventSub e)) wlst
                         modify (worldActor.workers .~ wlst')
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
