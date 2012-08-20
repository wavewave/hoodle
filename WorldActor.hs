{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables #-}

----------------------------
-- | describe world object
----------------------------

module WorldActor where 

import Control.Category
import Control.Monad.Error 
import Control.Monad.Reader
import Control.Monad.State
import Data.Lens.Common 
-- 
import Coroutine
import Event 
import Object
-- 
import Prelude hiding ((.),id)


-- | full state of world 
data WorldState = WorldState { _isDoorOpen :: Bool 
                             , _message :: String 
                              , _tempLog :: String -> String 
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
emptyWorldState :: WorldState 
emptyWorldState = WorldState False "" id


type WorldObject m r = ServerObj SubOp (StateT (WorldAttrib m) m) r 


-- | full collection of actors in world 
data WorldActor m 
    = WorldActor { _objDoor :: WorldObject m () 
                 , _objMessageBoard :: WorldObject m () 
                 } 

-- | isDoorOpen lens
objDoor :: Lens (WorldActor m) (ServerObj SubOp (StateT (WorldAttrib m) m) ())
objDoor = lens _objDoor (\b a -> a { _objDoor = b })


-- | messageBoard lens
objMessageBoard :: Lens (WorldActor m) (ServerObj SubOp (StateT (WorldAttrib m) m) ()) 
objMessageBoard = lens _objMessageBoard (\b a-> a { _objMessageBoard = b })

-- | 
initWorldActor :: (Monad m) => WorldActor m 
initWorldActor = WorldActor { _objDoor = door, _objMessageBoard = messageBoard }

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


-- | door object 
door :: (Monad m) => ServerObj SubOp (StateT (WorldAttrib m) m) () 
door = ReaderT doorW 
  where doorW (Input GiveEventSub ev) = do 
          case ev of 
            Open -> do b <- getState isDoorOpen -- lift( liftM (getL (isDoorOpen.worldState)) get )
                       when (not b) $ do 
                         putState isDoorOpen True 
                         modState tempLog (. (++ "door opened\n")) 
                      {-   lift (put . setL (isDoorOpen.worldState) True 
                                   . modL (tempLog.worldState) (. (++"door opened\n"))
                                   =<< get) -}
                       req <- request (Output GiveEventSub ())
                       doorW req
            Close -> do -- lift (put . setL (isDoorOpen.worldState) False =<< get)
                        b <- getState isDoorOpen
                        when b $ do 
                          putState isDoorOpen False
                          modState tempLog (. (++ "door closed\n"))
                        req <- request (Output GiveEventSub ()) 
                        doorW req 
            Render -> do 
                         req <- request (Output GiveEventSub ()) 
                         doorW req 
            _ -> do req <- request Ignore 
                    doorW req 


-- | 
messageBoard :: Monad m => ServerObj SubOp (StateT (WorldAttrib m) m) ()
messageBoard = ReaderT msgbdW
  where msgbdW (Input GiveEventSub ev) = do 
          case ev of 
            Message msg -> do lift (put . setL (message.worldState) msg =<< get) 
                              req <- request (Output GiveEventSub ())
                              msgbdW req
            _ -> do req <- request Ignore 
                    msgbdW req 

