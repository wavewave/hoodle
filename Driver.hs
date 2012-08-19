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
{-   driverW :: (Monad m, MonadLog m, MonadIO m) => 
           LogServer (DriverT m) () 
        -> World (DriverT m) () 
        -> DrvInput -> DriverT m ()  -}
    driverW logobj worldobj (Input Dispatch ev) = do 
      Right (logobj',worldobj') <- case ev of
        Message str -> runErrorT $ do 
          (logobj',_) <- logobj `connectE` (writeLog str)
          (worldobj',_) <- worldobj `connectE` (giveEvent ev)
          (worldobj'',_) <- worldobj' `connectE` render 
          return (logobj',worldobj')
      req <- request (Output Dispatch ()) 
      driverW logobj' worldobj' req 

-- | convenience routine for driver 
fire :: (Monad m, MonadLog m) => Event -> EStT (Driver m ()) m () --  -> ErrorT String (StateT (Driver m ()) m) ()
fire = query . dispatch  

