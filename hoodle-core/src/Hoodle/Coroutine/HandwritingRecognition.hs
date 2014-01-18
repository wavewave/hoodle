{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.HandwritingRecognition
-- Copyright   : (c) 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.HandwritingRecognition where

import           Control.Monad.Trans (liftIO)
-- 
import           Hoodle.Coroutine.Minibuffer
import           Hoodle.Type.Coroutine

handwritingRecognitionTest :: MainCoroutine ()
handwritingRecognitionTest = do
  liftIO $ putStrLn "handwriting recognition test here"
  r <- minibufDialog "test handwriting recognition"
  liftIO $ print r
  return ()
  
