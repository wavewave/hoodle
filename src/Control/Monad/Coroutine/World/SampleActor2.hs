{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables #-}

----------------------------
-- | describe world object
----------------------------

module Control.Monad.Coroutine.World.SampleActor2 where 

import Control.Category
import Control.Concurrent 
import Control.Monad.Error 
import Control.Monad.Reader
import Control.Monad.State
import Data.Lens.Common 
-- 
import Control.Monad.Coroutine
import Control.Monad.Coroutine.Event 
import Control.Monad.Coroutine.Object
import Control.Monad.Coroutine.Queue 
-- 
import Prelude hiding ((.),id)

data JobStatus = None | Started | Ended 
               deriving (Show,Eq)


-- | full state of world 
data WorldState = WorldState { _jobStatus :: JobStatus
                             , _tempLog :: String -> String 
                             , _tempQueue :: Queue (Either ActionOrder Event)
                             }

-- | isDoorOpen lens
jobStatus :: Lens WorldState JobStatus
jobStatus = lens _jobStatus (\d s -> s { _jobStatus = d })

-- | 
tempLog :: Lens WorldState (String -> String) 
tempLog = lens _tempLog (\b a -> a { _tempLog = b } )

-- | 
tempQueue :: Lens WorldState (Queue (Either ActionOrder Event))
tempQueue = lens _tempQueue (\b a -> a { _tempQueue = b} )

-- | 
emptyWorldState :: WorldState 
emptyWorldState = WorldState None id emptyQueue 


type WorldObject m r = ServerObj SubOp (StateT (WorldAttrib m) m) r 


-- | full collection of actors in world 
data WorldActor m 
    = WorldActor { _objWorker :: WorldObject m () 
                 } 

-- | isDoorOpen lens
objWorker :: Lens (WorldActor m) (ServerObj SubOp (StateT (WorldAttrib m) m) ())
objWorker = lens _objWorker (\b a -> a { _objWorker = b })

-- | 
initWorldActor :: (Monad m) => WorldActor m 
initWorldActor = WorldActor { _objWorker = worker
                            }

-- | 
data WorldAttrib m =
       WorldAttrib { _worldState :: WorldState
                   , _worldActor :: WorldActor m } 


-- | lens
worldState :: Lens (WorldAttrib m) WorldState
worldState = lens _worldState (\b a -> a {_worldState = b})

-- | lens 
worldActor :: Lens (WorldAttrib m) (WorldActor m)
worldActor = lens _worldActor (\b a -> a {_worldActor = b})

-- | initialization   
initWorld :: (Monad m) => WorldAttrib m 
initWorld = WorldAttrib emptyWorldState initWorldActor


-- | 
data SubOp i o where 
  GiveEventSub :: SubOp Event ()

-- |
giveEventSub :: (Monad m) => Event -> ClientObj SubOp m () 
giveEventSub ev = request (Input GiveEventSub ev) >> return ()

getState l = lift ( liftM ( getL (l.worldState) ) get )

putState l x = lift ( put . ( setL (l.worldState) x ) =<<  get  )

modState l m = lift ( put . modL (l.worldState) m =<< get ) 


worker :: (Monad m) => ServerObj SubOp (StateT (WorldAttrib m) m) ()
worker = ReaderT workerW 
  where workerW (Input GiveEventSub ev) = do 
          r <- case ev of 
                 Start -> do 
                   modState tempQueue . enqueue . Left . ActionOrder $ 
                     \evhandler -> do 
                        forkIO $ do threadDelay 10000000
                                    putStrLn "BAAAAAMM"
                                    evhandler Finished
                        return ()
                   putState jobStatus Started
                   return True
                 Finished -> do 
                   putState jobStatus Ended 
                   return True 
                 Render -> do 
                   st <- getState jobStatus
                   modState tempLog (. (++ "job status = " ++ show st ++ "\n"))
                   return True
                 _ -> return False 
          req <- if r then request (Output GiveEventSub ())
                      else request Ignore 
          workerW req 
          
                    

{-

-- | air object 
air :: (Monad m) => ServerObj SubOp (StateT (WorldAttrib m) m) () 
air = ReaderT airW 
  where airW (Input GiveEventSub ev) = do 
          r <- case ev of 
                 Sound snd -> do 
                   modState tempLog (. (++ "sound " ++ snd ++"\n"))
                   modState tempQueue . enqueue . Left . ActionOrder $ 
                     \evhandler -> do 
                        forkIO $ do threadDelay 10000000
                                    putStrLn "BAAAAAMM"
                                    evhandler (Message "HAHAHAH")
                        return ()
                   return True
                 _ -> return False 
          req <- if r then request (Output GiveEventSub ())
                      else request Ignore 
          airW req 


-- | door object 
door :: (Monad m) => ServerObj SubOp (StateT (WorldAttrib m) m) () 
door = ReaderT doorW 
  where doorW (Input GiveEventSub ev) = do 
          r <- case ev of 
                Open   -> do 
                  b <- getState isDoorOpen 
                  when (not b) $ do 
                    putState isDoorOpen True 
                    modState tempLog (. (++ "door opened\n")) 
                  return True 
                Close  -> do
                  b <- getState isDoorOpen
                  when b $ do 
                    putState isDoorOpen False
                    modState tempLog (. (++ "door closed\n"))
                    modState tempQueue (enqueue (Right (Sound "bam!")))
                  return True
                Render -> do 
                  b <- getState isDoorOpen
                  modState tempLog 
                           (. (++ "current door state : " ++ show b ++ "\n"))
                  return True
                _ -> return False
          req <- if r then request (Output GiveEventSub ())
                      else request Ignore
          doorW req 
  
-- | 
messageBoard :: Monad m => ServerObj SubOp (StateT (WorldAttrib m) m) ()
messageBoard = ReaderT msgbdW
  where msgbdW (Input GiveEventSub ev) = do 
          r <- case ev of 
                 Message msg -> do putState message msg 
                                   return True 
                 Render      -> do msg <- getState message 
                                   modState tempLog 
                                     (.(++ "current msg : " ++ msg ++ "\n"))
                                   return True 
                 _           -> return False 
          req <- if r then request (Output GiveEventSub ())
                      else request Ignore 
          msgbdW req

-}