{-# LANGUAGE GADTs, FlexibleInstances, ScopedTypeVariables #-}

----------------------------
-- | IO event driverzo
--
----------------------------

module Control.Monad.Trans.Crtn.Driver where 

import Control.Monad.Error
import Control.Monad.Reader
import Data.Foldable
-- 
import Control.Monad.Trans.Crtn 
import Control.Monad.Trans.Crtn.Event 
import Control.Monad.Trans.Crtn.IOActor 
import Control.Monad.Trans.Crtn.Logger 
import Control.Monad.Trans.Crtn.Object
import Control.Monad.Trans.Crtn.World  

-- | signature of IO event driver
data DrvOp e i o where 
  Dispatch :: DrvOp e e () 


-- -- | event driver input 
-- type DrvInput e = Arg (DrvOp e)

-- -- | driver server monad 
-- type DriverT = ServerT DrvOp 

-- | driver 
type Driver e m = SObjT (DrvOp e) m  

-- | driver client 
type DrvClient e m r = CObjT (DrvOp e) m r 

-- | 
dispatch :: (Monad m) => e -> DrvClient e m () 
dispatch ev = do request (Arg Dispatch ev) 
                 return ()

-- | basic driver 
                 
driver :: forall m e. (Monad m, MonadLog m, MonadIO m) => 
           LogServer (SObjBT (DrvOp e) m) ()
           -> SObjT (WorldOp e (SObjBT (DrvOp e) m)) (SObjBT (DrvOp e) m) () 
           -> (e -> IO ()) 
           -> Driver e m ()  
driver logger world evhandler = 
    ReaderT (driverW logger world (ioactorgen evhandler)) 
  where 
    driverW :: LogServer (SObjBT (DrvOp e) m) () 
            -> SObjT (WorldOp e (SObjBT (DrvOp e) m)) (SObjBT (DrvOp e) m) () 
            -> SObjT (IOOp e) (SObjBT (DrvOp e) m) () 
            -> Arg (DrvOp e)
            -> SObjBT (DrvOp e) m () 
    
    driverW logobj worldobj ioactorobj (Arg Dispatch ev) = do 
      (logobj',worldobj',ioactorobj') <- multiDispatchTillEnd (logobj,worldobj,ioactorobj) [Right ev]
      req <- request (Res Dispatch ()) 
      driverW logobj' worldobj' ioactorobj' req 



-- | single event dispatch 
singleDispatch :: Monad m => 
                  Either (ActionOrder e) e
               -> (LogServer m (), World e m (), IOActor e m (), [Either (ActionOrder e) e])
               -> m (LogServer m (), World e m (), IOActor e m (), [Either (ActionOrder e) e])
singleDispatch (Right ev) (logobj,worldobj,ioactorobj,evacc) = do
    Right (logobj',worldobj',events) <- 
      runErrorT $ do (worldobj1,_)  <- worldobj  <==| giveEvent ev
                     (worldobj2,logobj1) <- worldobj1 <==| flushLog logobj
                     (worldobj3,events) <- worldobj2 <==| flushQueue 
                     return (logobj1,worldobj3,events)
    return (logobj',worldobj',ioactorobj,evacc++events) 
singleDispatch (Left (ActionOrder act)) (logobj,worldobj,ioactorobj,evacc) = do 
     Right ioactorobj' <- 
       runErrorT $ do (ioactorobj',_) <- ioactorobj <==| doIOAction act
                      return ioactorobj'
     return (logobj,worldobj,ioactorobj',evacc) 


-- | a single feedback step of multiple event dispatch
multiDispatch :: Monad m => 
                 (LogServer m (), World e m (), IOActor e m ())
              -> [Either (ActionOrder e) e]
              -> m (LogServer m (), World e m (), IOActor e m (), [Either (ActionOrder e) e])
multiDispatch (logobj,worldobj,ioactorobj) events = do 
  foldrM singleDispatch (logobj,worldobj,ioactorobj,[]) events   

-- | full multiple event dispatch with feedback
multiDispatchTillEnd :: Monad m => 
                        (LogServer m (), World e m (), IOActor e m ()) 
                     -> [Either (ActionOrder e) e] 
                     -> m (LogServer m (), World e m (), IOActor e m ())
multiDispatchTillEnd (logobj,worldobj,ioactorobj) events = 
    go (logobj,worldobj,ioactorobj,events)
  where go (l,w,io,evs) = do  
          (l',w',io',evs') <- multiDispatch (l,w,io) evs 
          if (not.null) evs' 
            then go (l',w',io',evs')
            else return (l',w',io')
          

-- | convenience routine for driver 
fire :: (Monad m, MonadLog m) => e -> EStT (Driver e m ()) m () 
fire = query . dispatch  


