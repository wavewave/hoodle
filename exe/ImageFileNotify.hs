module ImageFileNotify where

import Control.Concurrent
import Control.Monad (forever)
import Filesystem.Path
import System.FSNotify 
import System.IO (getLine)
-- 
import Prelude hiding (FilePath)

actpred (Added _ _) = True
actpred _ = False

workChan :: Chan Event -> IO () 
workChan chan =  
  forever $ do 
    ev <- readChan chan 
    print ev 

-- act = print

startImageFileNotify :: Chan Event -> FilePath -> IO ()
startImageFileNotify chan fp = do 
  print fp
  withManager $ \wm -> do 
    putStrLn "watching start"
    watchTreeChan wm fp actpred chan
    forever getLine