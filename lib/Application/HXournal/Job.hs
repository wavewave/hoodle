module Application.HXournal.Job where

import Application.HXournal.Coroutine
import Application.HXournal.GUI

import Data.IORef


startJob :: IO () 
startJob = do 
  putStrLn "job started"
  startGUI 


startCoroutineTest = do 
  putStrLn "generator test"
  r <- run tram 
  print r 
  --
  -- eventprocessor atestcallback 
  tref <- newIORef tram
  eventprocessor (bouncecallback tref)

