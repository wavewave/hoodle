{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables #-}

----------------------------
-- | describe world object
----------------------------

module Control.Monad.Coroutine.WorldActor where 

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


-- | full state of world 
data WorldState = WorldState { _isDoorOpen :: Bool 
                             , _message :: String 
                             , _tempLog :: String -> String 
                             , _tempQueue :: Queue (Either ActionOrder Event)
                             }

-- | isDoorOpen lens
isDoorOpen :: Lens WorldState Bool
isDoorOpen = lens _isDoorOpen (\d s -> s { _isDoorOpen = d })

-- | messageBoard lens
message :: Lens WorldState String 
message = lens _message (\str st -> st { _message = str })

-- | 
tempLog :: Lens WorldState (String -> String) 
tempLog = lens _tempLog (\b a -> a { _tempLog = b } )

-- | 
tempQueue :: Lens WorldState (Queue (Either ActionOrder Event))
tempQueue = lens _tempQueue (\b a -> a { _tempQueue = b} )

-- | 
emptyWorldState :: WorldState 
emptyWorldState = WorldState False "" id emptyQueue 


type WorldObject m r = ServerObj SubOp (StateT (WorldAttrib m) m) r 


-- | full collection of actors in world 
data WorldActor m 
    = WorldActor { _objDoor :: WorldObject m () 
                 , _objMessageBoard :: WorldObject m () 
                 , _objAir :: WorldObject m () 
                 } 

-- | isDoorOpen lens
objDoor :: Lens (WorldActor m) (ServerObj SubOp (StateT (WorldAttrib m) m) ())
objDoor = lens _objDoor (\b a -> a { _objDoor = b })


-- | messageBoard lens
objMessageBoard :: Lens (WorldActor m) (ServerObj SubOp (StateT (WorldAttrib m) m) ()) 
objMessageBoard = lens _objMessageBoard (\b a-> a { _objMessageBoard = b })

-- | air lens 
objAir :: Lens (WorldActor m) (ServerObj SubOp (StateT (WorldAttrib m) m) ())
objAir = lens _objAir (\b a -> a { _objAir = b } )

-- | 
initWorldActor :: (Monad m) => WorldActor m 
initWorldActor = WorldActor { _objDoor = door
                            , _objMessageBoard = messageBoard 
                            , _objAir = air 
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

-- const (putStrLn "BAAAAAMM")
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

