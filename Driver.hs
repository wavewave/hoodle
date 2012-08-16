{-# LANGUAGE GADTs, FlexibleInstances #-}

----------------------------
-- | IO event driver
--
----------------------------

module Driver where 

import Control.Monad.Trans
-- 
import Coroutine 
import Event 
import Logger 
import Object
import World  

-- | signature of IO event driver
data DrvOp i o where 
  Dispatch :: DrvOp Event () 
  GetEvent :: DrvOp () Event 

-- | event driver input 
type DrvInput = MethodInput DrvOp

-- | driver server monad 
type DriverT = ServerT DrvOp 

-- | driver 
type Driver m = ServerObj DrvOp m  

-- | driver client 
type DrvClient m r = ClientObj DrvOp m r 

-- | 
dispatch :: (Monad m) => Event -> DrvClient m () 
dispatch ev = do request (Input Dispatch ev) 
                 return ()

-- | basic driver 
driver :: (Monad m, MonadLog m, MonadIO m) => Driver m () 
driver = Server (driverW logger world) 

-- | for testing internal state 
driverW :: (Monad m, MonadLog m, MonadIO m) => 
           LogServer (DriverT m) () 
        -> World (DriverT m) () 
        -> DrvInput -> DriverT m () 
driverW logobj worldobj (Input Dispatch ev) = do 
    (logobj',worldobj') <- case ev of
      Message str -> do 
        Just (logobj',_) <- logobj `connect` (writeLog str)
        Just (worldobj',_) <- worldobj `connect` (giveEvent ev)
        Just (worldobj'',_) <- worldobj' `connect` render 
        return (logobj',worldobj')
 
    req <- request (Output Dispatch ()) 
    driverW logobj' worldobj' req 
    -- driverW logobj req 

-- | convenience routine for driver 
fire :: (Monad m, MonadLog m) => Event -> EStT (Driver m ()) m () --  -> ErrorT String (StateT (Driver m ()) m) ()
fire = query . dispatch  

