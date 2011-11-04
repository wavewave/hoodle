module Application.HXournal.Command where

import Application.HXournal.ProgType
import Application.HXournal.Job

commandLineProcess :: Hxournal -> IO ()
commandLineProcess (Test fname) = do 
  putStrLn "test called"
  startJob fname
commandLineProcess (MakeSVG fname) = do 
  putStrLn "makeSVG is called"
  startMakeSVG fname

