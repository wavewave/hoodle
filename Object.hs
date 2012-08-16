{-# LANGUAGE GADTs, RankNTypes, ImpredicativeTypes #-}

module Object where 

import Control.Monad.State 
import Control.Monad.Error 
--
import Coroutine 

-- | input of method of an object with signature s 
data MethodInput s = forall i o. Input (s i o) i 

-- | output of methdo of an object with signature s 
data MethodOutput s = forall i o.  Output (s i o) o 

-- | Server object
type ServerObj s m r = Server (MethodInput s) (MethodOutput s) m r 
 
-- | Client object
type ClientObj s m r = Client (MethodInput s) (MethodOutput s) m r 


-- | 
query :: (Monad m) => ClientObj s m r 
      -> ErrorT String (StateT (ServerObj s m ()) m) r 
query cli = do 
  qserv <- lift get 
  (qserv',r) <- mapErrorT lift (qserv `connectE` cli )
  lift (put qserv')
  return r


