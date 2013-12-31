{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Job 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Hoodle.Job where

import Hoodle.Script.Hook

import Hoodle.GUI
-- import Hoodle.Builder



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
