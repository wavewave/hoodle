{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables #-}

----------------------------
-- | describe world object
----------------------------

module World where 

import Control.Applicative
import Control.Category
import Control.Monad.Error 
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans 
import Data.Lens.Common 
-- 
import Coroutine
import Event 
import Object
-- 
import Prelude hiding ((.),id)

-- | full state of world 
data WorldState = WorldState { _isDoorOpen :: Bool 
                             , _message :: String }

-- | isDoorOpen lens
isDoorOpen :: Lens WorldState Bool
isDoorOpen = lens _isDoorOpen (\d s -> s { _isDoorOpen = d })

-- | messageBoard lens
message :: Lens WorldState String 
message = lens _message (\str st -> st { _message = str })

-- | 
emptyWorldState = WorldState False "" 


-- | full collection of actors in world 
data WorldActor m 
    = WorldActor { _objDoor :: ServerObj SubOp (StateT (WorldAttrib m) m) () 
                 , _objMessageBoard :: ServerObj SubOp (StateT (WorldAttrib m) m) ()
                 } 

-- | isDoorOpen lens
objDoor :: Lens (WorldActor m) (ServerObj SubOp (StateT (WorldAttrib m) m) ())
objDoor = lens _objDoor (\b a -> a { _objDoor = b })


-- | messageBoard lens
objMessageBoard :: Lens (WorldActor m) (ServerObj SubOp (StateT (WorldAttrib m) m) ()) 
objMessageBoard = lens _objMessageBoard (\b a-> a { _objMessageBoard = b })

initWorldActor = WorldActor { _objDoor = door, _objMessageBoard = messageBoard }

data WorldAttrib m =
       WorldAttrib { _worldState :: WorldState
                   , _worldActor :: WorldActor m } 

worldState = lens _worldState (\b a -> a {_worldState = b})

worldActor = lens _worldActor (\b a -> a {_worldActor = b})


initWorld = WorldAttrib emptyWorldState initWorldActor

-- | 
data WorldOp i o where 
  GiveEvent :: WorldOp Event () 
  Render :: WorldOp () () 

-- | 
type World m r = ServerObj WorldOp m r  


-- | 
giveEvent :: (Monad m) => Event -> ClientObj WorldOp m () 
giveEvent ev = request (Input GiveEvent ev) >> return () 


-- | 
render :: (Monad m) => ClientObj WorldOp m () 
render = request (Input Render ()) >> return ()




-- | 
world :: forall m. (MonadIO m) => ServerObj WorldOp m () 
world = ReaderT staction 
  where 
    staction req = do runStateT (worldW req) initWorld
                      return ()
    worldW (Input Render ()) = do 
      b <- getL (isDoorOpen.worldState) <$> get 
      str <- getL (message.worldState) <$> get 
      liftIO $ putStrLn str 
      liftIO $ putStrLn ("door open? " ++ show b)
      req <- lift (request (Output Render ()))
      worldW req 
    worldW (Input GiveEvent ev) = do
      dobj <- getL (objDoor.worldActor) <$> get  
      mobj <- getL (objMessageBoard.worldActor) <$> get 
      Right (dobj',mobj') <- runErrorT $ 
        do (dobj',_) <- dobj <==> giveEventSub ev
           (mobj',_) <- mobj <==> giveEventSub ev 
           return (dobj',mobj') 
      put . setL (objDoor.worldActor) dobj . setL (objMessageBoard.worldActor) mobj =<< get 
      req <- lift (request (Output GiveEvent ()))
      worldW req  

-- | 
data SubOp i o where 
  GiveEventSub :: SubOp Event ()

giveEventSub :: (Monad m) => Event -> ClientObj SubOp m () 
giveEventSub ev = request (Input GiveEventSub ev) >> return ()


-- | door object 
door :: (Monad m) => ServerObj SubOp (StateT (WorldAttrib m) m) () 
door = ReaderT doorW 
  where doorW (Input GiveEventSub ev) = do 
          case ev of 
            Open -> do lift (put . setL (isDoorOpen.worldState) True =<< get) 
                       req <- request (Output GiveEventSub ())
                       doorW req
            Close -> do lift (put . setL (isDoorOpen.worldState) False =<< get)
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

