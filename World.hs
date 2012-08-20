{-# LANGUAGE GADTs, NoMonomorphismRestriction #-}

----------------------------
-- | describe world object
----------------------------

module World where 

import Control.Applicative
import Control.Monad.Error 
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans 
import Data.Lens.Common 
-- 
import Coroutine
import Event 
import Object


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
world :: (MonadIO m) => ServerObj WorldOp m () 
world = ReaderT staction 
  where 
    staction req = runStateT (worldW door messageBoard req) emptyWorldState >> return ()
    worldW dobj mobj (Input Render ()) = do 
      b <- getL isDoorOpen <$> get 
      str <- getL message <$> get 
      liftIO $ putStrLn str 
      liftIO $ putStrLn ("door open? " ++ show b)
      req <- lift (request (Output Render ()))
      worldW dobj mobj req 
    worldW dobj mobj (Input GiveEvent ev) = do
      Right (dobj',mobj') <- runErrorT $ 
        do (dobj',_) <- dobj <==> giveEventSub ev
           (mobj',_) <- mobj <==> giveEventSub ev 
           return (dobj',mobj') 
      req <- lift (request (Output GiveEvent ()))
      worldW dobj' mobj' req 

-- | 
data SubOp i o where 
  GiveEventSub :: SubOp Event ()

giveEventSub :: (Monad m) => Event -> ClientObj SubOp m () 
giveEventSub ev = request (Input GiveEventSub ev) >> return ()


-- | door object 
door :: (Monad m) => ServerObj SubOp (StateT WorldState m) () 
door = ReaderT doorW 
  where doorW (Input GiveEventSub ev) = do 
          case ev of 
            Open -> do lift (put . setL isDoorOpen True =<< get) 
                       req <- request (Output GiveEventSub ())
                       doorW req
            Close -> do lift (put . setL isDoorOpen False =<< get)
                        req <- request (Output GiveEventSub ()) 
                        doorW req 
            _ -> do req <- request Ignore 
                    doorW req 


-- | 
messageBoard :: Monad m => ServerObj SubOp (StateT WorldState m) ()
messageBoard = ReaderT msgbdW
  where msgbdW (Input GiveEventSub ev) = do 
          case ev of 
            Message msg -> do lift (put . setL message msg =<< get) 
                              req <- request (Output GiveEventSub ())
                              msgbdW req
            _ -> do req <- request Ignore 
                    msgbdW req 

