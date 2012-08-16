{-# LANGUAGE GADTs, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

----------------------------
-- | IO event driver
--
----------------------------

module Driver where 

import Control.Monad.Error
import Control.Monad.State 
import Control.Monad.Trans 
-- 
import Coroutine 
import Object 

-- | event 
data Event = Message String 

-- | signature of IO event driver
data DrvOp i o where 
  Dispatch :: DrvOp Event () 

-- | event driver input 
type DrvInput = MethodInput DrvOp

-- | driver 
type Driver m r = ServerObj DrvOp m r 

-- | driver client 
type DrvClient m r = ClientObj DrvOp m r 

-- | 
dispatch :: (Monad m) => Event -> DrvClient m () 
dispatch ev = do request (Input Dispatch ev) 
                 return ()

-------------------------
-- Logging monad 
-------------------------

class (Monad m) => MonadLog m where 
    scribe :: String -> m () 
  
instance MonadLog IO where
    scribe = putStrLn 

instance (MonadIO m) => MonadLog m where
    scribe = liftIO . putStrLn 


-- | basic driver 
driver :: (Monad m, MonadLog m) => Driver m () 
driver = driverW 0 

-- | for testing internal state 
driverW :: (Monad m, MonadLog m) => Int -> Driver m () 
driverW i (Input Dispatch ev) = do 
    case ev of
      Message str -> lift (scribe $ show i ++ "th event : " ++ str)
    req <- request (Output Dispatch ()) 
    driverW (i+1) req 


-- | convenience routine for driver 
fire :: (Monad m, MonadLog m) => Event 
        -> ErrorT String (StateT (Driver m ()) m) ()
fire = query . dispatch  
