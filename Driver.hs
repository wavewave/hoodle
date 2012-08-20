{-# LANGUAGE GADTs, FlexibleInstances #-}

----------------------------
-- | IO event driver
--
----------------------------

module Driver where 

import Control.Monad.Error
import Control.Monad.Reader
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
driver = ReaderT (driverW logger world) 
  where 
    driverW logobj worldobj (Input Dispatch ev) = do 
      Right (logobj',worldobj') <- 
        runErrorT $ do (logobj1,_)    <- logobj    <==> writeLog ("[Driver] " ++ show ev)
                       (worldobj1,_)  <- worldobj  <==> giveEvent ev
                       -- (worldobj'',_) <- worldobj' <==> render 
                       (worldobj2,logobj2) <- worldobj1 <==> flushLog logobj1
                       return (logobj2,worldobj2)
      req <- request (Output Dispatch ()) 
      driverW logobj' worldobj' req 

-- | convenience routine for driver 
fire :: (Monad m, MonadLog m) => Event -> EStT (Driver m ()) m () 
fire = query . dispatch  


