module Control.Monad.Coroutine.EventHandler where 
  
import Control.Concurrent.MVar 
import Control.Monad.State 
import Control.Monad.Error
-- 
import Control.Monad.Coroutine
import Control.Monad.Coroutine.Event 
import Control.Monad.Coroutine.Driver 
import Control.Monad.Coroutine.Logger 

eventHandler :: MVar (Driver IO ()) -> Event -> IO ()
eventHandler dref ev = do 
    drv <- takeMVar dref 
    eaction drv >>= either (\err -> scribe (show err) >> return drv) return >>= putMVar dref 
  where eaction :: Driver IO () -> IO (Either (CoroutineError ()) (Driver IO ()))
        eaction = evalStateT $ runErrorT $ fire ev >> lift get >>= return 

  