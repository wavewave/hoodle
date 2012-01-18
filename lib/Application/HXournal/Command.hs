-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Command 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Application.HXournal.Command where


import Application.HXournal.ProgType
import Application.HXournal.Job

commandLineProcess :: Hxournal -> IO ()
commandLineProcess (Test mfname) = do 
  putStrLn "test called"
  startJob mfname
  
