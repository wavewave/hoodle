-- |
-- Module      : Hoodle.Command
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
module Hoodle.Command where

import Hoodle.Job (startJob)
import Hoodle.ProgType (Hoodle (..))
import Hoodle.Script.Hook (Hook (..))

commandLineProcess :: Hoodle -> Maybe Hook -> IO ()
commandLineProcess (Test mfname) mhook = do
  startJob mfname mhook
