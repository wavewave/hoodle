-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Script.Hook
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.Script.Hook where 


import Data.Xournal.Simple 


data Hook = Hook { 
              saveAsHook :: Xournal -> IO ()
            } 
