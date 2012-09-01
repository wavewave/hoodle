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
data DrvOp i o where 
  Dispatch :: DrvOp Event () 


-- | event driver input 
type DrvInput = Arg DrvOp

-- -- | driver server monad 
-- type DriverT = ServerT DrvOp 

-- | driver 
type Driver m = SObjT DrvOp m  

-- | driver client 
type DrvClient m r = CObjT DrvOp m r 

-- | 
dispatch :: (Monad m) => Event -> DrvClient m () 
dispatch ev = do request (Arg Dispatch ev) 
                 return ()

-- | basic driver 
driver :: forall m. (Monad m, MonadLog m, MonadIO m) => 
           LogServer (SObjBT DrvOp m) ()
        -> SObjT (WorldOp (SObjBT DrvOp m)) (SObjBT DrvOp m) () -> (Event -> IO ()) -> Driver m () 
driver logger world evhandler = 
    ReaderT (driverW logger world (ioactorgen evhandler)) 
  where 
    driverW logobj worldobj ioactorobj (Arg Dispatch ev) = do 
      (logobj',worldobj',ioactorobj') <- multiDispatchTillEnd (logobj,worldobj,ioactorobj) [Right ev]
      req <- request (Res Dispatch ()) 
      driverW logobj' worldobj' ioactorobj' req 

    -- driverW :: LogServer (SObjBT DrvOp m) () 
    --            -> SObjT (WorldOp (SObjBT DrvOp m)) (SObjBT DrvOp m) () 
    --           -> SObjT IOOp (SObjBT DrvOp m) () 
    --            -> Arg DrvOp 
    --           -> SObjBT DrvOp m () 


-- | single event dispatch 
singleDispatch :: Monad m => 
                  Either ActionOrder Event 
               -> (LogServer m (), World m (), IOActor m (), [Either ActionOrder Event])
               -> m (LogServer m (), World m (), IOActor m (), [Either ActionOrder Event])
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
                 (LogServer m (), World m (), IOActor m ())
              -> [Either ActionOrder Event]
              -> m (LogServer m (), World m (), IOActor m (), [Either ActionOrder Event])
multiDispatch (logobj,worldobj,ioactorobj) events = do 
  foldrM singleDispatch (logobj,worldobj,ioactorobj,[]) events   

-- | full multiple event dispatch with feedback
multiDispatchTillEnd :: Monad m => 
                        (LogServer m (), World m (), IOActor m ()) 
                     -> [Either ActionOrder Event] 
                     -> m (LogServer m (), World m (), IOActor m ())
multiDispatchTillEnd (logobj,worldobj,ioactorobj) events = 
    go (logobj,worldobj,ioactorobj,events)
  where go (l,w,io,evs) = do  
          (l',w',io',evs') <- multiDispatch (l,w,io) evs 
          if (not.null) evs' 
            then go (l',w',io',evs')
            else return (l',w',io')
          

-- | convenience routine for driver 
fire :: (Monad m, MonadLog m) => Event -> EStT (Driver m ()) m () 
fire = query . dispatch  


