{-# LANGUAGE GADTs, FlexibleInstances, ScopedTypeVariables #-}

----------------------------
-- | IO event driverzo
--
----------------------------

module Control.Monad.Coroutine.Driver where 

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Foldable
-- 
import Control.Monad.Coroutine 
import Control.Monad.Coroutine.Event 
import Control.Monad.Coroutine.IOActor 
import Control.Monad.Coroutine.Logger 
import Control.Monad.Coroutine.Object
import Control.Monad.Coroutine.World  

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
driver :: forall m. (Monad m, MonadLog m, MonadIO m) => 
          ServerObj (WorldOp (ServerT DrvOp m)) (ServerT DrvOp m) () -> (Event -> IO ()) -> Driver m () 
driver world evhandler = ReaderT (driverW logger world (ioactorgen evhandler)) 
  where 
    driverW :: LogServer (ServerT DrvOp m) () -> ServerObj (WorldOp (ServerT DrvOp m)) (ServerT DrvOp m) () -> ServerObj IOOp (ServerT DrvOp m) () -> MethodInput DrvOp -> ServerT DrvOp m () 
    driverW logobj worldobj ioactorobj (Input Dispatch ev) = do 
      (logobj',worldobj',ioactorobj') <- multiDispatchTillEnd (logobj,worldobj,ioactorobj) [Right ev]
      req <- request (Output Dispatch ()) 
      driverW logobj' worldobj' ioactorobj' req 


-- | single event dispatch 
singleDispatch (Right ev) (logobj,worldobj,ioactorobj,evacc) = do
    Right (logobj',worldobj',events) <- 
      runErrorT $ do (logobj1,_)    <- logobj    <==> writeLog ("[Driver] " ++ show ev)
                     (worldobj1,_)  <- worldobj  <==> giveEvent ev
                     (worldobj2,logobj2) <- worldobj1 <==> flushLog logobj1
                     (worldobj3,events) <- worldobj2 <==> flushQueue 
                     return (logobj2,worldobj3,events)
    return (logobj',worldobj',ioactorobj,evacc++events) 
singleDispatch (Left (ActionOrder act)) (logobj,worldobj,ioactorobj,evacc) = do 
     Right ioactorobj' <- 
       runErrorT $ do (ioactorobj',_) <- ioactorobj <==> doIOAction act
                      return ioactorobj'
     return (logobj,worldobj,ioactorobj',evacc) 


-- | a single feedback step of multiple event dispatch
multiDispatch (logobj,worldobj,ioactorobj,evacc) events = do 
  foldrM singleDispatch (logobj,worldobj,ioactorobj,[]) events   

-- | full multiple event dispatch with feedback
multiDispatchTillEnd (logobj,worldobj,ioactorobj) events = 
    go (logobj,worldobj,ioactorobj,events)
  where go (l,w,io,evs) = do  
          (l',w',io',evs') <- multiDispatch (l,w,io,[]) evs 
          if (not.null) evs' 
            then go (l',w',io',evs')
            else return (l',w',io')
          

-- | convenience routine for driver 
fire :: (Monad m, MonadLog m) => Event -> EStT (Driver m ()) m () 
fire = query . dispatch  


