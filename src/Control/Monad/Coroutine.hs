{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, 
             MultiParamTypeClasses, UndecidableInstances #-}

module Control.Monad.Coroutine where 

import Control.Monad.Error
import Control.Monad.Reader 
import Control.Monad.State 
import Control.Monad.Trans.Free

----------------------------
-- added instance of FreeT monad
----------------------------

instance (Monad m, MonadState st m, Functor f) => MonadState st (FreeT f m) where 
  get = lift get
  put = lift . put

---------------------------
-- general generator 
---------------------------

-- | yield command functor
data Yield o x = Yield o x 

instance Functor (Yield o) where 
  fmap f (Yield o x) = Yield o (f x)

-- | Generator type is single-sided coroutine which only gives an output 
--   without getting any request. 
type Generator o = FreeT (Yield o)

-- | primitive coroutine action yielding an output
yield :: (Monad m) => o -> Generator o m ()  
yield o = wrap (Yield o (return ()))

---------------------------
-- general consumer
---------------------------

-- | await command functor for consumer coroutine
newtype Await a x = Await (a->x)

-- | 
instance Functor (Await g) where 
  fmap f (Await g) = Await (f.g)

-- | Consumer type is a single-sided coroutine which only gets an input 
--   without producing output. 
type Consumer a = FreeT (Await a) 

-- | primitive coroutine action awaiting an input
await :: (Monad m) => Consumer a m a
await = wrap (Await return)

----------------------------
-- general coroutine
----------------------------

-- | command functor of general bidirectional coroutine
data Request req ans x = Request req (ans -> x)

instance Functor (Request req ans) where
  fmap f (Request req g) = Request req (f.g)

-- | general symmetric bidirectional coroutine
type Coroutine req ans = FreeT (Request req ans)

-- | primitive request coroutine  
request :: Monad m => req -> Coroutine req ans m ans
request r = wrap (Request r return)

-------------------------------
-- server/client model
------------------------------

-- | Server type 
type Server req ans m = ReaderT req (Coroutine ans req m) 

-- | Coroutine type is regarded as a Client type 
--   which can be paired with Server type with opposite request 
--   and answer type.  
type Client = Coroutine   

-- | type for coroutine status after execution
data CoroutineError r = NoError 
                      | ServerFinished r 
                      | Other String 

-- | 
deriving instance (Show r) => Show (CoroutineError r) 

-- | 
instance Error (CoroutineError r) where 
  noMsg = NoError 
  strMsg str = Other str 

-----------------------------
-- communication combinator 
-------------------------------

-- | connecting server and client in error monad
connectE :: Monad m => 
            Server req ans m r'    -- ^ server coroutine
         -> Client req ans m r  -- ^ client coroutine
         -> ErrorT (CoroutineError r') m (Server req ans m r', r)
connectE s c = do 
    y <- lift (runFreeT c)
    case y of
      Pure r -> return (s,r)
      Free (Request rq af) -> do 
        x <- lift (runFreeT (runReaderT s rq))
        case x of 
          Pure r' -> throwError (ServerFinished r')
          Free (Request ans rf) -> connectE (ReaderT rf) (af ans)


-- | synonym of connectE
(<==>) :: Monad m => Server req ans m r' -> Coroutine req ans m r
          -> ErrorT (CoroutineError r') m (Server req ans m r', r)
(<==>) = connectE
           
