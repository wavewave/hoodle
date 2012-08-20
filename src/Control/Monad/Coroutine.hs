{-# LANGUAGE StandaloneDeriving #-}

module Control.Monad.Coroutine where 

import Control.Monad.Error
import Control.Monad.Reader 
import Control.Monad.Trans
import Control.Monad.Trans.Free


---------------------------
-- general generator 
---------------------------

data Yield o x = Yield o x 

instance Functor (Yield o) where 
  fmap f (Yield o x) = Yield o (f x)

type Generator o = FreeT (Yield o)

yield :: (Monad m) => o -> Generator o m ()  
yield o = wrap (Yield o (return ()))

---------------------------
-- general consumer
---------------------------

newtype Await a x = Await (a->x)

instance Functor (Await g) where 
  fmap f (Await g) = Await (f.g)

type Consumer a = FreeT (Await a) 

await :: (Monad m) => Consumer a m a
await = wrap (Await return)


----------------------------
-- general coroutine
----------------------------

data Request req ans x = Request req (ans -> x)

instance Functor (Request req ans) where
  fmap f (Request req g) = Request req (f.g)

type Coroutine req ans = FreeT (Request req ans)

-- | general request 
request :: Monad m => req -> Coroutine req ans m ans
request r = wrap (Request r return)


-- -- | type alias for Server 
-- newtype Server req ans m r = Server { server :: req -> Coroutine ans req m r} 

type Server req ans m = ReaderT req (Coroutine ans req m) 

-- instance (Monad m) => Monad (Server req ans m) where 
--   return 
 
-- | type alias for Client 
type Client req ans = Coroutine req ans  

{-
-- | connect two coroutine loop. First coroutine is a server and second coroutine is a client.--   Connection initiated by starting client first. 
connect :: (Monad m) => 
           Server req ans m ()    -- ^ server coroutine
        -> Coroutine req ans m r  -- ^ client coroutine
        -> m (Maybe (Server req ans m (), r))
connect s c = do 
    y <- runFreeT c           
    case y of
      Pure r -> return (Just (s,r))
      Free (Request rq af) -> do 
        x <- runFreeT (runReaderT s rq)
        case x of 
          Pure _ -> return Nothing
          Free (Request ans rf) -> connect (ReaderT rf) (af ans)
-}

data CoroutineError r = NoError | ServerFinished r | Other String 

deriving instance (Show r) => Show (CoroutineError r) 

instance Error (CoroutineError r) where 
  noMsg = NoError 
  strMsg str = Other str 

-- | connecting server and client in error monad
connectE :: Monad m => 
            Server req ans m r'    -- ^ server coroutine
         -> Coroutine req ans m r  -- ^ client coroutine
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
           

{-
-- | infinite request/response action for server, note that request and answer are reverted in server 
--   compared with client.  
serveForever :: (Monad m) => (ans -> Coroutine req ans m ans) -> ans -> Coroutine req ans m ans
serveForever action ans = foldM (flip ($)) ans (repeat action) 
-}