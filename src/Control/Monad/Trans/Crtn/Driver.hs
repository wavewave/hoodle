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
-- import Control.Monad.Trans.Crtn.IOActor 
import Control.Monad.Trans.Crtn.Logger 
import Control.Monad.Trans.Crtn.Object
import Control.Monad.Trans.Crtn.World  

-- | signature of IO event driver
data DrvOp e i o where 
  Dispatch :: DrvOp e e (Maybe (ActionOrder e)) -- () 


-- -- | event driver input 
-- type DrvInput e = Arg (DrvOp e)

-- -- | driver server monad 
-- type DriverT = ServerT DrvOp 

-- | driver 
type Driver e m = SObjT (DrvOp e) m  

-- | driver client 
type DrvClient e m r = CObjT (DrvOp e) m r 

-- | 
dispatch :: (Monad m) => e -> DrvClient e m (Maybe (ActionOrder e)) --  () 
dispatch ev = do Res Dispatch r <- request (Arg Dispatch ev) 
                 return r
              
-- | basic driver 
                 
driver :: forall m e. (Monad m, MonadLog m, MonadIO m) => 
           LogServer (SObjBT (DrvOp e) m) ()
           -> SObjT (WorldOp e (SObjBT (DrvOp e) m)) (SObjBT (DrvOp e) m) () 
           -> Driver e m ()  
driver logger world = 
    ReaderT (driverW logger world) 
  where 
    driverW :: LogServer (SObjBT (DrvOp e) m) () 
            -> SObjT (WorldOp e (SObjBT (DrvOp e) m)) (SObjBT (DrvOp e) m) () 
            -> Arg (DrvOp e)
            -> SObjBT (DrvOp e) m () 
    driverW logobj worldobj (Arg Dispatch ev) = do 
      (logobj',worldobj') <- multiDispatchTillEnd (logobj,worldobj) [Right ev]
      req <- request (Res Dispatch Nothing) 
      driverW logobj' worldobj' req 



-- | single event dispatch 
singleDispatch :: (Monad m) =>  
                  Either (ActionOrder e) e
               -> ( LogServer (SObjBT (DrvOp e) m) ()
                  , World e (SObjBT (DrvOp e) m) ()
                  , [EvOrAct e])
               -> SObjBT (DrvOp e) m 
                    ( LogServer (SObjBT (DrvOp e) m) ()
                    , World e (SObjBT (DrvOp e) m) ()
                    , [EvOrAct e])
singleDispatch (Right ev) (logobj,worldobj,evacc) = do
    Right (logobj',worldobj',events) <- 
      runErrorT $ do (worldobj1,_)  <- worldobj  <==| giveEvent ev
                     (worldobj2,logobj1) <- worldobj1 <==| flushLog logobj
                     (worldobj3,events) <- worldobj2 <==| flushQueue 
                     return (logobj1,worldobj3,events)
    return (logobj',worldobj',evacc++events) 
singleDispatch (Left act) (logobj,worldobj,evacc) = do 
    Arg Dispatch ev <- request (Res Dispatch (Just act))
    return (logobj,worldobj,evacc++[Right ev]) 

 -- (ioactorobj',_) <- ioactorobj <==| doIOAction act
                      -- return ioactorobj'


-- | a single feedback step of multiple event dispatch
multiDispatch :: Monad m => 
                 ( LogServer (SObjBT (DrvOp e) m) ()
                 , World e (SObjBT (DrvOp e) m) ())
              -> [EvOrAct e]
              -> SObjBT (DrvOp e) m 
                   ( LogServer (SObjBT (DrvOp e) m) ()
                   , World e  (SObjBT (DrvOp e) m) ()
                   , [EvOrAct e] )
multiDispatch (logobj,worldobj) events = do 
  foldrM singleDispatch (logobj,worldobj,[]) events   

-- | full multiple event dispatch with feedback
multiDispatchTillEnd :: Monad m => 
                        ( LogServer (SObjBT (DrvOp e) m) ()
                        , World e (SObjBT (DrvOp e) m) ()) 
                     -> [EvOrAct e] 
                     -> SObjBT (DrvOp e) m 
                          (LogServer (SObjBT (DrvOp e) m) ()
                          , World e  (SObjBT (DrvOp e) m) ())
multiDispatchTillEnd (logobj,worldobj) events = 
    go (logobj,worldobj,events)
  where go (l,w,evs) = do  
          (l',w',evs') <- multiDispatch (l,w) evs 
          if (not.null) evs' 
            then go (l',w',evs')
            else return (l',w')
          

-- | convenience routine for driver 
fire :: (Monad m, MonadLog m) => e -> EStT (Driver e m ()) m 
                                           (Maybe (ActionOrder e)) 
fire = query . dispatch  


