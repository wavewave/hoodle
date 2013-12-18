{-# LANGUAGE OverloadedStrings #-}

module ImageFileNotify where

import Control.Concurrent
import Control.Monad (forever)
import DBus
import DBus.Client 
import Filesystem.Path
import Filesystem.Path.CurrentOS
import System.FSNotify 
import System.IO (getLine)
-- 
import Prelude hiding (FilePath)

actpred (Added _ _) = True
actpred _ = False

workChan :: Client -> Chan Event -> IO () 
workChan cli chan =  
  forever $ do 
    ev <- readChan chan 
    case ev of 
      Added fp _ -> do 
        case (toText fp) of
          Right txt -> emit cli (signal "/image" "org.ianwookim.hoodle" "filepath") { signalBody = [toVariant txt] }
          _ -> return ()
      _ -> return ()

-- act = print

startImageFileNotify :: Chan Event -> FilePath -> IO ()
startImageFileNotify chan fp = do 
  print fp
  withManager $ \wm -> do 
    putStrLn "watching start"
    watchTreeChan wm fp actpred chan
    forever getLine