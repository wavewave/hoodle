{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.Hoodle.Job 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Application.Hoodle.Job where

import Application.Hoodle.Script.Hook

import Application.Hoodle.GUI
-- import Application.Hoodle.Builder



startJob :: Maybe FilePath -> Maybe Hook -> IO () 
startJob mfname mhook = do 
  putStrLn "job started"
  startGUI mfname mhook 

{-
startTestBuilder :: FilePath -> IO () 
startTestBuilder fname = do 
  putStrLn fname
  xojcontent <- read_xojgz fname 
  L.writeFile "mytest.xoj" $ builder xojcontent
-}
