{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Hoodle.Job
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
module Hoodle.Job where

import Hoodle.GUI (startGUI)
import Hoodle.Script.Hook (Hook)

startJob :: Maybe FilePath -> Maybe Hook -> IO ()
startJob mfname mhook = do
  putStrLn "job started"
  startGUI mfname mhook
