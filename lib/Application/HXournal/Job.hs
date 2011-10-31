module Application.HXournal.Job where

import Application.HXournal.GUI
import Application.HXournal.Iteratee 
import Application.HXournal.Type 
import Data.IORef
import Control.Monad.Coroutine
import Control.Monad.State

startJob :: IO () 
startJob = do 
  putStrLn "job started"
  startGUI