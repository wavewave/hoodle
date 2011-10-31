module Application.HXournal.Iteratee where 

import Control.Monad
import Control.Monad.State
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.IO.Class

import Application.HXournal.Type
import Application.HXournal.Util

iter :: Iteratee MyEvent XournalStateIO () 
iter = do liftIO (putStrLn "I am waiting first result") 
          sequence_ (repeat changepage)
          return ()

changepage :: Iteratee MyEvent XournalStateIO ()
changepage = do 
  r1 <- await 
  case r1 of 
    ButtonLeft -> do 
      st <- lift get 
      lift (put (st-1))
      liftIO . putStrLn $ "changing " ++ show st ++ " to " ++ show (st-1)
    ButtonRight -> do 
      st <- lift get 
      lift (put (st+1))
      liftIO . putStrLn $ "changing " ++ show st ++ " to " ++ show (st+1)
    ButtonRefresh -> do 
      liftIO . putStrLn $ "refresh"
    ButtonQuit -> do  
      liftIO . putStrLn $ "quit"

