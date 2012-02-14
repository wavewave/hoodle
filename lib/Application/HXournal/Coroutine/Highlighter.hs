-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Coroutine.Highlighter 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.Coroutine.Highlighter where

import Application.HXournal.Device 
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Coroutine.Pen 

import Control.Monad.Trans

-- | 

highlighterStart :: CanvasId -> PointerCoord -> MainCoroutine () 
highlighterStart cid pcoord = do 
  liftIO $ putStrLn "highlighter started"
  penStart cid pcoord
