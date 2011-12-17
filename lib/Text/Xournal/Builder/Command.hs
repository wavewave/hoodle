module Text.Xournal.Builder.Command where

import Text.Xournal.Builder.Type
import Text.Xournal.Builder.Job

commandLineProcess :: Xournal_builder -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob
