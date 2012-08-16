{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, GADTs, Rank2Types #-}

module QServer ( 
  QInput, 
  QOutput, 
  QServer, 
  QClient, 
  addQ, 
  retrieveQ, 
  qserver, 
  query 
) where 

import Control.Monad.State
import Control.Monad.Trans.Error
-- 
import Coroutine
import Object 
import Queue 

-- | signature of queue operation 
data QOp a i o where 
  Add      :: QOp a a () 
  Retrieve :: QOp a () (Maybe a)

-- | Queue operation input definition
type QInput a = MethodInput (QOp a) 

-- | Queue operation input definition
type QOutput a = MethodOutput (QOp a) 

-- | QServer definition
type QServer a m r = ServerObj (QOp a) m r 

-- | QClient definition 
type QClient a m r = ClientObj (QOp a) m r 

--------------------------------
-- QClient primitive actions
--------------------------------

-- | add an element to Queue
addQ :: (Monad m) => a -> QClient a m ()
addQ e = do request (Input Add e)
            return () 

-- | retrieve an element from Queue 
retrieveQ :: (Monad m) => QClient a m (Maybe a)
retrieveQ = do r <- request (Input Retrieve ()) 
               case r of 
                 Output Retrieve me -> return me 
                 _ -> error "retrieveQ"   -- Allow partiality here  
 

-- | basic queue server 
qserver :: (Monad m) => QServer a m () 
qserver = go (Queue [] []) 
  where 
    go :: (Monad m) => Queue a -> QServer a m () 
    go q req = 
      case req of 
        Input Add e       -> do let q' = enqueue e q
                                req' <- request (Output Add ())
                                go q' req'
        Input Retrieve () -> do let (q',my) = dequeue q 
                                req' <- request (Output Retrieve my)
                                go q' req'


-- | 
query :: (Monad m) => QClient a m r 
      -> ErrorT String (StateT (QServer a m ()) m) r 
query cli = do 
  qserv <- lift get 
  (qserv',r) <- mapErrorT lift (qserv `connectE` cli )
  lift (put qserv')
  return r
