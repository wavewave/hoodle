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

import           Control.Lens
import           Control.Monad.State 
-- from hoodle-platform
import           Data.Hoodle.Simple
-- from this package
import qualified Hoodle.Script.Hook as H
import           Hoodle.Type.Coroutine
import           Hoodle.Type.HoodleState
-- 
import Prelude hiding ((.),id)

-- |

afterSaveHook :: Hoodle -> MainCoroutine ()
afterSaveHook hdl = do 
  xstate <- get 
  let aftersavehk = do         
        hset <- view hookSet xstate 
        H.afterSaveHook hset
  maybe (return ()) (\f -> liftIO (f hdl)) aftersavehk      

-- | 
  
saveAsHook :: Hoodle -> MainCoroutine ()
saveAsHook hdl = do 
  xstate <- get 
  let saveashk = do         
        hset <- view hookSet xstate 
        H.saveAsHook hset
  maybe (return ()) (\f -> liftIO (f hdl)) saveashk      


