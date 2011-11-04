module Application.HXournal.Command where

import Application.HXournal.ProgType
import Application.HXournal.Job

commandLineProcess :: Hxournal -> IO ()
commandLineProcess (Test fname) = do 
  putStrLn "test called"
  startJob fname
{-
commandLineProcess CoroutineTest = do 
  putStrLn "coroutine test is called"
  startCoroutineTest
-}