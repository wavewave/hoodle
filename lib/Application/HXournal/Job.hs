{-# LANGUAGE ScopedTypeVariables #-}

module Application.HXournal.Job where

import Application.HXournal.GUI
-- import Application.HXournal.Builder
import qualified Data.ByteString.Lazy as L
import Text.Xournal.Parse

startJob :: Maybe FilePath -> IO () 
startJob mfname = do 
  putStrLn "job started"
  startGUI mfname

{-
startTestBuilder :: FilePath -> IO () 
startTestBuilder fname = do 
  putStrLn fname
  xojcontent <- read_xojgz fname 
  L.writeFile "mytest.xoj" $ builder xojcontent
-}
