{-# LANGUAGE GADTs, FlexibleInstances #-}

----------------------------
-- | IO event driver
--
----------------------------

module Driver where 

import Control.Monad.Trans
-- 
import Coroutine 
import Logger 
import Object 

-- | event 
data Event = Message String 

-- | signature of IO event driver
data DrvOp i o where 
  Dispatch :: DrvOp Event () 

-- | event driver input 
type DrvInput = MethodInput DrvOp

-- | driver server monad 
type DriverM = ServerM DrvOp 

-- | driver 
type Driver m = ServerObj DrvOp m  

-- | driver client 
type DrvClient m r = ClientObj DrvOp m r 

-- | 
dispatch :: (Monad m) => Event -> DrvClient m () 
dispatch ev = do request (Input Dispatch ev) 
                 return ()

-- | basic driver 
driver :: (Monad m, MonadLog m) => Driver m () 
driver = Server (driverW logger)

-- | for testing internal state 
driverW :: (Monad m, MonadLog m) => LogServer (DriverM m) () -> DrvInput -> DriverM m () 
driverW logobj (Input Dispatch ev) = do 
    Just (logobj',_) <- case ev of
                          Message str -> (logobj `connect` (writeLog str)) 
    req <- request (Output Dispatch ()) 
    driverW logobj' req 
    -- driverW logobj req 

-- | convenience routine for driver 
fire :: (Monad m, MonadLog m) => Event -> EStT (Driver m ()) m () --  -> ErrorT String (StateT (Driver m ()) m) ()
fire = query . dispatch  

