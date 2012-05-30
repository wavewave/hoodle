-----------------------------------------------------------------------------
-- |
-- Module      : Application.Hoodle.Command 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.Hoodle.Command where


import Application.Hoodle.ProgType
import Application.Hoodle.Job
import Application.Hoodle.Script.Hook

commandLineProcess :: Hoodle -> Maybe Hook -> IO ()
commandLineProcess (Test mfname) mhook = do 
  startJob mfname mhook
  
