{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Script.Coroutine
-- Copyright   : (c) 2012-2015 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Script.Coroutine where

import           Control.Lens (view)
import           Control.Monad.State 
import           Control.Monad.Trans.Maybe
-- from hoodle-platform
import           Data.Hoodle.Simple
-- from this package
import qualified Hoodle.Script.Hook as H
import           Hoodle.Type.Coroutine
import           Hoodle.Type.HoodleState
-- 

-- | 
afterSaveHook :: FilePath -> Hoodle -> MainCoroutine ()
afterSaveHook fp hdl = do 
  xstate <- get 
  let aftersavehk = do         
        hset <- view hookSet xstate 
        H.afterSaveHook hset
  maybe (return ()) (\f -> liftIO (f fp hdl)) aftersavehk      

-- | 
saveAsHook :: FilePath -> Hoodle -> MainCoroutine ()
saveAsHook _fp hdl = do 
  xstate <- get 
  let saveashk = do         
        hset <- view hookSet xstate 
        H.saveAsHook hset
  maybe (return ()) (\f -> liftIO (f hdl)) saveashk      

hoist :: (Monad m) => Maybe a -> MaybeT m a 
hoist = MaybeT . return 

-- |
recentFolderHook :: MainCoroutine (Maybe FilePath) 
recentFolderHook = do 
  xstate <- get 
  (r :: Maybe FilePath) <- runMaybeT $ do 
    hset <- hoist (view hookSet xstate)
    rfolder <- hoist (H.recentFolderHook hset)
    liftIO rfolder
  return r 

-- | 
embedPredefinedImageHook :: MainCoroutine (Maybe FilePath) 
embedPredefinedImageHook = do 
  xstate <- get 
  (r :: Maybe FilePath) <- runMaybeT $ do 
    hset <- hoist (view hookSet xstate)
    rfilename <- hoist (H.embedPredefinedImageHook hset)
    liftIO rfilename 
  return r 
  
-- | temporary
embedPredefinedImage2Hook :: MainCoroutine (Maybe FilePath) 
embedPredefinedImage2Hook = do 
  xstate <- get 
  (r :: Maybe FilePath) <- runMaybeT $ do 
    hset <- hoist (view hookSet xstate)
    rfilename <- hoist (H.embedPredefinedImage2Hook hset)
    liftIO rfilename 
  return r 
  
-- | temporary
embedPredefinedImage3Hook :: MainCoroutine (Maybe FilePath) 
embedPredefinedImage3Hook = do 
  xstate <- get 
  (r :: Maybe FilePath) <- runMaybeT $ do 
    hset <- hoist (view hookSet xstate)
    rfilename <- hoist (H.embedPredefinedImage3Hook hset)
    liftIO rfilename 
  return r   

