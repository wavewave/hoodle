-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Script.Coroutine
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Script.Coroutine where

import           Control.Monad.Trans
import           Data.Label
-- from hoodle-platform
import           Data.Xournal.Simple
-- from this package
import           Hoodle.Accessor
import qualified Hoodle.Script.Hook as H
import           Hoodle.Type.Coroutine
import           Hoodle.Type.XournalState
-- 
import Prelude hiding ((.),id)

-- |

afterSaveHook :: Xournal -> MainCoroutine ()
afterSaveHook xoj = do 
  xstate <- getSt 
  let aftersavehk = do         
        hset <- get hookSet xstate 
        H.afterSaveHook hset
  maybe (return ()) (\f -> liftIO (f xoj)) aftersavehk      

-- | 
  
saveAsHook :: Xournal -> MainCoroutine ()
saveAsHook xoj = do 
  xstate <- getSt 
  let saveashk = do         
        hset <- get hookSet xstate 
        H.saveAsHook hset
  maybe (return ()) (\f -> liftIO (f xoj)) saveashk      


