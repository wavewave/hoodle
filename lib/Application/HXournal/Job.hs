{-# LANGUAGE ScopedTypeVariables #-}

module Application.HXournal.Job where

import Application.HXournal.GUI
import Application.HXournal.Builder
import qualified Data.ByteString.Lazy as L
import Text.Xournal.Parse

startJob :: FilePath -> IO () 
startJob fname = do 
  putStrLn "job started"
  startGUI fname

startTestBuilder :: FilePath -> IO () 
startTestBuilder fname = do 
  putStrLn fname
  xojcontent <- read_xojgz fname 
  L.writeFile "mytest.xoj" $ builder xojcontent
  