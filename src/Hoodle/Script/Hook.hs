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

import System.FilePath 
import Data.Hoodle.Simple 

-- | 
data Hook = Hook { saveAsHook :: Maybe (Hoodle -> IO ())
                 , afterSaveHook :: Maybe (FilePath -> Hoodle -> IO ())
                 , afterOpenHook :: Maybe (FilePath -> Hoodle -> IO ())
                 , afterUpdateClipboardHook :: Maybe ([Item] -> IO ())
                 , customContextMenuTitle :: Maybe String 
                 , customContextMenuHook :: Maybe ([Item] -> IO ())
                 , fileNameSuggestionHook :: Maybe (IO String) 
                 , recentFolderHook :: Maybe (IO FilePath)
                 } 


defaultHook :: Hook 
defaultHook = Hook { saveAsHook = Nothing
                   , afterSaveHook = Nothing  
                   , afterUpdateClipboardHook = Nothing
                   , customContextMenuTitle = Nothing 
                   , customContextMenuHook = Nothing 
                   , fileNameSuggestionHook = Nothing 
                   , recentFolderHook = Nothing 
                   }
