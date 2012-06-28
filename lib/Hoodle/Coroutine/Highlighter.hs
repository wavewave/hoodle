-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Highlighter 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Highlighter where

import Hoodle.Device 
import Hoodle.Type.Coroutine
import Hoodle.Type.Canvas
import Hoodle.Coroutine.Pen 

import Control.Monad.Trans

-- | 

highlighterStart :: CanvasId -> PointerCoord -> MainCoroutine () 
highlighterStart cid pcoord = do 
  liftIO $ putStrLn "highlighter started"
  penStart cid pcoord
