-----------------------------------------------------------------------------
-- |
-- Module      : Application.Hoodle.Coroutine.Highlighter 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.Hoodle.Coroutine.Highlighter where

import Application.Hoodle.Device 
import Application.Hoodle.Type.Coroutine
import Application.Hoodle.Type.Canvas
import Application.Hoodle.Coroutine.Pen 

import Control.Monad.Trans

-- | 

highlighterStart :: CanvasId -> PointerCoord -> MainCoroutine () 
highlighterStart cid pcoord = do 
  liftIO $ putStrLn "highlighter started"
  penStart cid pcoord
