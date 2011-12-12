module Application.HXournal.Command where

import Application.HXournal.ProgType
import Application.HXournal.Job

commandLineProcess :: Hxournal -> IO ()
commandLineProcess (Test mfname) = do 
  putStrLn "test called"
  startJob mfname
  