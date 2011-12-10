{-# LANGUAGE FlexibleContexts #-}

module Application.HXournal.Coroutine where

import Control.Monad.Coroutine 
import Control.Monad.State
import Control.Monad.Coroutine.SuspensionFunctors
import Data.IORef

import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Event 

dummycallback :: MyEvent -> IO ()
dummycallback = const (return ())

-- IORef (Await MyEvent (Iteratee MyEvent XournalStateIO ())) 
--                -> IORef HXournalState

bouncecallback :: TRef -> SRef -> MyEvent -> IO () 
bouncecallback tref sref input = do 
  Await cont <- readIORef tref 
  st <- readIORef sref
  (nr,st') <- runStateT (resume (cont input)) st 
  case nr of  
    Left  naw -> do writeIORef tref naw 
                    writeIORef sref st'
    Right val -> do putStrLn $ show val 
                    writeIORef tref (Await (\_ -> return ()))
                    writeIORef sref st'
  return ()  

