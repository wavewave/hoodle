{-# LANGUAGE GADTs, RankNTypes #-}

module Control.Monad.Coroutine.Object where 

import Control.Monad.State 
import Control.Monad.Error 
--
import Control.Monad.Coroutine 

-- | input of method of an object with signature s 
data MethodInput s = forall i o. Input (s i o) i 

-- | output of methdo of an object with signature s 
data MethodOutput s = forall i o.  Output (s i o) o 
                    | Ignore

type ServerT s = Coroutine (MethodOutput s) (MethodInput s)  



-- | Server object
type ServerObj s m = Server (MethodInput s) (MethodOutput s) m  
 
-- | Client object
type ClientObj s = Client (MethodInput s) (MethodOutput s)  


-- | convenient error state monad for object  
type EStT s m = ErrorT (CoroutineError ()) (StateT s m)  

-- |  
query :: (Monad m) => ClientObj s m r -> EStT (ServerObj s m ()) m r  
query cli = do 
  qserv <- lift get 
  (qserv',r) <- mapErrorT lift (qserv `connectE` cli )
  lift (put qserv')
  return r


