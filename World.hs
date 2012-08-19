{-# LANGUAGE GADTs, NoMonomorphismRestriction #-}

----------------------------
-- | describe world object
----------------------------

module World where 

import Control.Monad.Reader
import Control.Monad.Trans 
-- import Data.Lens.Common 
-- 
import Coroutine
import Event 
import Object

{-
-- | full state of world 
data WorldState = WorldState { _isDoorOpen :: Bool 
                             , _messageBoard :: String }

-- | isDoorOpen lens
isDoorOpen :: Lens WorldState Bool
isDoorOpen = lens _isDoorOpen (\d s -> s { _isDoorOpen = d })

-- | messageBoard lens
messageBoard :: Lens WorldState String 
messageBoard = lens _messageBoard (\str st -> st { _messageBoard = str })
-}

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
world = ReaderT (worldW (False,"")) -- (const (return ()) . flip runStateT (False,"") . worldW)
  where 
    worldW (b,str) (Input Render ()) = do 
      liftIO $ putStrLn str 
      liftIO $ putStrLn ("door open? " ++ show b)
      req <- request (Output Render ())
      worldW (b,str) req 
    worldW (b,str) (Input GiveEvent ev) = do 
      let (b',str') = case ev of 
                        Message msg -> (b,msg)
                        Open        -> (True,str)
                        Close       -> (False,str) 
      req <- request (Output GiveEvent ())
      worldW (b',str') req 


{-  -- | 
door :: (MonadIO m) => WorldState -> ServerObj WorldOp m () 
door = ReaderT (doorW False
-}