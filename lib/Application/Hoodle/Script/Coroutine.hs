-----------------------------------------------------------------------------
-- |
-- Module      : Application.Hoodle.Script.Coroutine
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.Hoodle.Script.Coroutine where

import Application.Hoodle.Type.XournalState
import qualified Application.Hoodle.Script.Hook as H
import Application.Hoodle.Type.Coroutine
import Application.Hoodle.Accessor

import Control.Category 
import Control.Monad.Trans
import Data.Label
import Data.Xournal.Simple

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


