{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables, KindSignatures #-}

----------------------------
-- | describe world object
----------------------------

module World where 

import Control.Applicative
import Control.Category
import Control.Monad.Error 
import Control.Monad.Reader
import Control.Monad.State
import Data.Lens.Common 
-- 
import Coroutine
import Event 
import Object
import Logger 
import WorldActor
-- 
import Prelude hiding ((.),id)


-- | 
data WorldOp m i o where 
  GiveEvent :: WorldOp m Event ()
  FlushLog :: WorldOp m (LogServer m ()) (LogServer m ())
  -- Render :: WorldOp m () () 

-- | 
type World m r = ServerObj (WorldOp m) m r  


-- | 
giveEvent :: (Monad m) => Event -> ClientObj (WorldOp m) m () 
giveEvent ev = request (Input GiveEvent ev) >> return () 


-- | 
flushLog :: (Monad m) => LogServer m () -> ClientObj (WorldOp m) m (LogServer m ()) 
flushLog logger = do req <- request (Input FlushLog logger) 
                     case req of 
                       Output FlushLog logger' -> return logger' 
                       _ -> error "error in flushLog"  -- allow partiality


{- -- | 
render :: (Monad m) => ClientObj (WorldOp m) m () 
render = request (Input Render ()) >> return ()
-}



-- | 
world :: forall m. (MonadIO m) => ServerObj (WorldOp m) m () 
world = ReaderT staction 
  where 
    staction req = do runStateT (go req) initWorld
                      return ()
    go :: (MonadIO m) => MethodInput (WorldOp m) -> StateT (WorldAttrib (ServerT (WorldOp m) m)) (ServerT (WorldOp m) m) () 
    go (Input GiveEvent ev) = do
      dobj <- getL (objDoor.worldActor) <$> get  
      mobj <- getL (objMessageBoard.worldActor) <$> get 
      Right (dobj',mobj') <- 
        runErrorT $ (,) <$> liftM fst (dobj <==> giveEventSub ev) 
                        <*> liftM fst (mobj <==> giveEventSub ev) 
      put . setL (objDoor.worldActor) dobj' 
          . setL (objMessageBoard.worldActor) mobj' =<< get  
      req <- lift (request (Output GiveEvent ()))
      go req   
    go (Input FlushLog (logobj :: LogServer m ())) = do
      logf <- getL (tempLog.worldState) <$> get 
      let l1 = runErrorT (logobj <==> writeLog ("[World] " ++ (logf ""))) 

      Right (logobj',_) <- lift . lift $ l1 
      req <- lift (request (Output FlushLog logobj'))
      go req  
 

-- :: m (Either (CoroutineError ()) (LogServer m (),()))
      -- liftIO $ putStrLn "flushlog called"
--      Right (logobj',_) <- runErrorT (logobj <==> writeLog ("[World] " ++ (logf "") ) ) :: Double 


--    go (Input Render ()) = do 
--      b <- getL (isDoorOpen.worldState) <$> get 
--      str <- getL (message.worldState) <$> get 
--      liftIO $ putStrLn str 
 --     liftIO $ putStrLn ("door open? " ++ show b)
--      req <- lift (request (Output Render ()))
--      go req 
