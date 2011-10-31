module Application.HXournal.Command where

import Application.HXournal.Type
import Application.HXournal.Job

commandLineProcess :: Hxournal -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob
commandLineProcess CoroutineTest = do 
  putStrLn "coroutine test is called"
  startCoroutineTest
