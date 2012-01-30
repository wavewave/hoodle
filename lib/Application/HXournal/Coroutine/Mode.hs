-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Coroutine.Mode 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.Coroutine.Mode where

import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.XournalState
import Application.HXournal.Accessor
--import Data.Foldable
import Data.Traversable
import Control.Monad.Trans
import Control.Category
import Data.Label
import Data.Xournal.Generic
import Graphics.Xournal.Render.BBoxMapPDF
import Prelude hiding ((.),id, mapM_, mapM)

modeChange :: MyEvent -> MainCoroutine () 
modeChange command = case command of 
                       ToViewAppendMode -> updateXState select2edit
                       ToSelectMode     -> updateXState edit2select 
                       _ -> return ()
  where select2edit xst =  
          either (noaction xst) (whenselect xst) . xojstateEither . get xournalstate $ xst
        edit2select xst = 
          either (whenedit xst) (noaction xst) . xojstateEither . get xournalstate $ xst
        noaction xstate = const (return xstate)
        whenselect xstate txoj = return . flip (set xournalstate) xstate 
                                 . ViewAppendState . GXournal (get g_selectTitle txoj)
                                 =<< liftIO (mapM resetPageBuffers (get g_selectAll txoj))
        whenedit xstate xoj = return . flip (set xournalstate) xstate 
                              . SelectState  
                              $ GSelect (get g_title xoj) (gpages xoj) Nothing

{-
modeChange ToSelectMode = updateX 
  xstate <- getSt
  let xojstate = get xournalstate xstate
  case xojstate of 
    ViewAppendState xoj -> do 
      liftIO $ putStrLn "to select mode"
      putSt
        $ xstate  
    SelectState _ -> return ()

-}

{-          
            let pages = get g_selectAll txoj 
            newpages <- liftIO (mapM resetPageBuffers . get_gselectAll)
        $ either (const (return ())) $ \txoj -> do
            liftIO $ putStrLn "to view append mode"
            let pages = get g_selectAll txoj 
            newpages <- liftIO $ mapM resetPageBuffers pages 
      putSt 
        . set xournalstate (ViewAppendState (GXournal (get g_selectTitle txoj) newpages ))
        $ xstate  -}
