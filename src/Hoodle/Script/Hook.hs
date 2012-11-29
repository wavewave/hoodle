-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Script.Hook
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Script.Hook where 

import Data.Hoodle.Simple 

-- | 
data Hook = Hook { saveAsHook :: Maybe (Hoodle -> IO ())
                 , afterSaveHook :: Maybe (Hoodle -> IO ())
                 , afterUpdateClipboardHook :: Maybe ([Item] -> IO ())
                 } 


