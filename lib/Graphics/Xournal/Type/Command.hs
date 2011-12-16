module Graphics.Xournal.Type.Command where

import Graphics.Xournal.Type.Type
import Graphics.Xournal.Type.Job

commandLineProcess :: Xournal_types -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob
