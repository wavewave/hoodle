module Application.HXournal.Job where

import Application.HXournal.Coroutine
import Application.HXournal.GUI

import Data.IORef

import Control.Monad.Coroutine

startJob :: IO () 
startJob = do 
  putStrLn "job started"
  startGUI 


startCoroutineTest = do 
  {-  putStrLn "generator test"
  r <- run tram 
  print r -}
  --
  -- eventprocessor atestcallback 

  r <- resume iter

  case r of 
    Left aw -> do 
      tref <- newIORef aw -- (Await (\_ -> iter))
      eventprocessor (bouncecallback tref)
    Right _ -> error "what?"

