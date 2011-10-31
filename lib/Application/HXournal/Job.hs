module Application.HXournal.Job where

import Application.HXournal.Coroutine
import Application.HXournal.GUI
import Application.HXournal.Iteratee 
import Data.IORef
import Control.Monad.Coroutine
import Control.Monad.State

startJob :: IO () 
startJob = do 
  putStrLn "job started"
  let st = (1 :: Int)
  (r,st') <- runStateT (resume iter) st
  sref <- newIORef st'

  case r of 
    Left aw -> do 
      tref <- newIORef aw 
      startGUI tref sref
    Right _ -> error "what?"

{-
startCoroutineTest = do 
  r <- resume iter
  let st = (1 :: Int)
  (r,st') <- runStateT (resume iter) st
  sref <- newIORef st' 

  case r of 
    Left aw -> do 
      tref <- newIORef aw 
      eventprocessor (bouncecallback tref sref)
    Right _ -> error "what?"
-}
