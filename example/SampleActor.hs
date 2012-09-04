{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables #-}

----------------------------
-- | describe world object
----------------------------

module SampleActor where 

import Control.Applicative
import Control.Category
import Control.Concurrent 
import Control.Monad.Error 
import Control.Monad.Reader
import Control.Monad.State
-- import Data.Lens.Common 
import Control.Lens 
-- 
import Control.Monad.Trans.Crtn
import Control.Monad.Trans.Crtn.Event 
import Control.Monad.Trans.Crtn.Object
import Control.Monad.Trans.Crtn.Queue 
-- 
import Event
-- 
import Prelude hiding ((.),id)


-- | full state of world 
data WorldState = WorldState { _isDoorOpen :: Bool 
                             , _message :: String 
                             , _tempLog :: String -> String 
                             , _tempQueue :: Queue (Either (ActionOrder Event) Event)
                             }

-- | isDoorOpen lens
isDoorOpen :: Simple Lens WorldState Bool
isDoorOpen = lens _isDoorOpen (\a b -> a { _isDoorOpen = b })

-- | messageBoard lens
message :: Simple Lens WorldState String 
message = lens _message (\a b -> a { _message = b })

-- | 
tempLog :: Simple Lens WorldState (String -> String) 
tempLog = lens _tempLog (\a b -> a { _tempLog = b } )

-- | 
tempQueue :: Simple Lens WorldState (Queue (Either (ActionOrder Event) Event))
tempQueue = lens _tempQueue (\a b -> a { _tempQueue = b} )

-- | 
emptyWorldState :: WorldState 
emptyWorldState = WorldState False "" id emptyQueue 


type WorldObject m r = SObjT SubOp (StateT (WorldAttrib m) m) r 


-- | full collection of actors in world 
data WorldActor m 
    = WorldActor { _objDoor :: WorldObject m () 
                 , _objMessageBoard :: WorldObject m () 
                 , _objAir :: WorldObject m () 
                 } 

-- | isDoorOpen lens
objDoor :: Simple Lens (WorldActor m) (SObjT SubOp (StateT (WorldAttrib m) m) ())
objDoor = lens _objDoor (\a b -> a { _objDoor = b })


-- | messageBoard lens
objMessageBoard :: Simple Lens (WorldActor m) (SObjT SubOp (StateT (WorldAttrib m) m) ()) 
objMessageBoard = lens _objMessageBoard (\a b -> a { _objMessageBoard = b })

-- | air lens 
objAir :: Simple Lens (WorldActor m) (SObjT SubOp (StateT (WorldAttrib m) m) ())
objAir = lens _objAir (\a b -> a { _objAir = b } )

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
worldState :: Simple Lens (WorldAttrib m) WorldState
worldState = lens _worldState (\a b -> a {_worldState = b})

-- | lens 
worldActor :: Simple Lens (WorldAttrib m) (WorldActor m)
worldActor = lens _worldActor (\a b -> a {_worldActor = b})

-- | initialization   
initWorld :: (Monad m) => WorldAttrib m 
initWorld = WorldAttrib emptyWorldState initWorldActor


-- | 
data SubOp i o where 
  GiveEventSub :: SubOp Event ()

-- |
giveEventSub :: (Monad m) => Event -> CObjT SubOp m () 
giveEventSub ev = request (Arg GiveEventSub ev) >> return ()

{-
getState l = lift ( liftM ( getL (l.worldState) ) get )

putState l x = lift ( put . ( setL (l.worldState) x ) =<<  get  )

modState l m = lift ( put . modL (l.worldState) m =<< get ) 
-}

-- | air object 
air :: (Monad m) => SObjT SubOp (StateT (WorldAttrib m) m) () 
air = ReaderT airW 
  where airW (Arg GiveEventSub ev) = do 
          r <- case ev of 
                 Sound s -> do 
                   modify ( worldState.tempLog %~ (. (++ "sound " ++ s ++"\n")))
                   let action = Left . ActionOrder $ 
                                  \evhandler -> do 
                                    forkIO $ do threadDelay 10000000
                                                putStrLn "BAAAAAMM"
                                                evhandler (Message "HAHAHAH")
                                    return ()
                   modify ( worldState.tempQueue %~ enqueue action ) 
                   return True
                 _ -> return False 
          req <- if r then request (Res GiveEventSub ())
                      else request Ign 
          airW req 


-- | door object 
door :: (Monad m) => SObjT SubOp (StateT (WorldAttrib m) m) () 
door = ReaderT doorW 
  where doorW (Arg GiveEventSub ev) = do 
          r <- case ev of 
                Open   -> do 
                  b <- (^. worldState.isDoorOpen) <$> get
                  when (not b) $ do 
                    modify (worldState.isDoorOpen .~ True )  
                    modify (worldState.tempLog %~ (. (++ "door opened\n")))
                  return True 
                Close  -> do
                  b <- (^. worldState.isDoorOpen) <$> get 
                  when b $ do 
                    modify (worldState.isDoorOpen .~ False)
                    modify (worldState.tempLog %~ (. (++ "door closed\n")))
                    modify (worldState.tempQueue %~ (enqueue (Right (Sound "bam!"))))
                  return True
                Render -> do 
                  b <- (^. worldState.isDoorOpen) <$> get 
                  modify (worldState.tempLog %~ (. (++ "current door state : " ++ show b ++ "\n")))
                  return True
                _ -> return False
          req <- if r then request (Res GiveEventSub ())
                      else request Ign
          doorW req 
  
-- | 
messageBoard :: Monad m => SObjT SubOp (StateT (WorldAttrib m) m) ()
messageBoard = ReaderT msgbdW
  where msgbdW (Arg GiveEventSub ev) = do 
          r <- case ev of 
                 Message msg -> do modify (worldState.message .~ msg)
                                   return True 
                 Render      -> do msg <- (^. worldState.message) <$> get 
                                   modify (worldState.tempLog %~
                                            (.(++ "current msg : " ++ msg ++ "\n")))
                                   return True 
                 _           -> return False 
          req <- if r then request (Res GiveEventSub ())
                      else request Ign 
          msgbdW req

