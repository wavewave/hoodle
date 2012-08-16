module EventHandler where 

import Control.Concurrent.MVar 
import Control.Monad 
import Control.Monad.State 
import Control.Monad.Error
-- 
import Driver 


eventHandler :: MVar (Driver IO ()) -> Event -> IO ()
eventHandler dref ev = do 
    drv <- takeMVar dref 
    eaction drv >>= either (\err -> scribe err >> return drv) return >>= putMVar dref 
  where eaction :: Driver IO () -> IO (Either String (Driver IO ()))
        eaction = evalStateT $ runErrorT $ fire ev >> lift get >>= return 

  