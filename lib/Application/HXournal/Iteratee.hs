module Application.HXournal.Iteratee where 

import Control.Monad
import Control.Monad.State
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.IO.Class

import Application.HXournal.Type
import Application.HXournal.Util

iter :: Iteratee MyEvent MyStateIO () 
iter = do liftIO (putStrLn "I am waiting first result") 
          sequence_ (repeat changepage)
{-          changepage
          changepage
          changepage -}
--          r1 <- await 
--           waitUntil (==5) check 
          r2 <- await 
          liftIO (putStrLn ("yeah... I got r2 = " ++ show r2))
          return ()

changepage :: Iteratee MyEvent MyStateIO ()
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
      liftIO . putStrLn $ "changing " ++ show st ++ " to " ++ show (st-1)
    ButtonRefresh -> do 
      liftIO . putStrLn $ "refresh"
    ButtonQuit -> do  
      liftIO . putStrLn $ "quit"

