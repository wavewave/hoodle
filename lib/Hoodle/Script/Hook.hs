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

import Data.Xournal.Simple 
import Data.Xournal.BBox


-- | 

data Hook = Hook { saveAsHook :: Maybe (Xournal -> IO ())
                 , afterSaveHook :: Maybe (Xournal -> IO ())
                 , afterUpdateClipboardHook :: Maybe ([StrokeBBox] -> IO ())
                 } 


