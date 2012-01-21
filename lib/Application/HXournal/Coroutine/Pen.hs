
-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Coroutine.Pen 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
module Application.HXournal.Coroutine.Pen where

import Graphics.UI.Gtk hiding (get,set,disconnect)
import Application.HXournal.Device 
import Application.HXournal.Type.Event
import Application.HXournal.Type.Enum
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Coroutine.Draw
import Application.HXournal.Coroutine.EventConnect
import Application.HXournal.Coroutine.Commit
import Application.HXournal.Accessor
import Application.HXournal.Util
import Application.HXournal.ModelAction.Pen
import Application.HXournal.ModelAction.Page
import Application.HXournal.Draw
import Control.Monad.Trans

import Data.Xournal.Predefined
import Data.Xournal.Generic
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Sequence hiding (filter)
import qualified Data.Map as M
import Data.Maybe 
import Control.Category
import Data.Label
import Prelude hiding ((.), id)
import Graphics.Xournal.Render.BBox

-- | enter pen drawing mode

penStart :: CanvasId -> PointerCoord -> MainCoroutine () 
penStart cid pcoord = do 
    xstate <- changeCurrentCanvasId cid 
    let cvsInfo = getCanvasInfo cid xstate 
    let currxoj = unView . get xournalstate $ xstate        
        pagenum = get currentPageNum cvsInfo
        pinfo = get penInfo xstate
        zmode = get (zoomMode.viewInfo) cvsInfo
    geometry <- getCanvasGeometry cvsInfo 
    let (x,y) = device2pageCoord geometry zmode pcoord 
    connidup   <- connectPenUp   cvsInfo 
    connidmove <- connectPenMove cvsInfo 

    pdraw <-penProcess cid geometry connidmove connidup (empty |> (x,y)) (x,y) 
    (newxoj,bbox) <- liftIO $ addPDraw pinfo currxoj pagenum pdraw
    let bbox' = inflate bbox (get (penWidth.currentTool.penInfo) xstate) 
        xstate' = set xournalstate (ViewAppendState newxoj) 
                  . updatePageAll (ViewAppendState newxoj)
                  $ xstate
    commit xstate'
    invalidateAll 
    -- mapM_ (flip invalidateInBBox bbox') . filter (/=cid) $ otherCanvas xstate' 


-- | main pen coordinate adding process

penProcess :: CanvasId
           -> CanvasPageGeometry
           -> ConnectId DrawingArea -> ConnectId DrawingArea 
           -> Seq (Double,Double) -> (Double,Double) 
           -> MainCoroutine (Seq (Double,Double))
penProcess cid cpg connidmove connidup pdraw (x0,y0) = do 
  r <- await 
  xstate <- getSt
  let cvsInfo = getCanvasInfo cid xstate
  case r of 
    PenMove _cid' pcoord -> do 
      let canvas = get drawArea cvsInfo
          zmode  = get (zoomMode.viewInfo) cvsInfo
          ptype  = get (penType.penInfo) xstate
          pcolor = get (penColor.currentTool.penInfo) xstate 
          pwidth = get (penWidth.currentTool.penInfo) xstate 
          (x,y) = device2pageCoord cpg zmode pcoord 
          (pcr,pcg,pcb,pca)= fromJust (M.lookup pcolor penColorRGBAmap) 
          opacity = case ptype of 
                      HighlighterWork -> predefined_highlighter_opacity 
                      _ -> 1.0
          pcolRGBA = (pcr,pcg,pcb,pca*opacity)
      liftIO $ drawSegment canvas cpg zmode pwidth pcolRGBA (x0,y0) (x,y)
      penProcess cid cpg connidmove connidup (pdraw |> (x,y)) (x,y) 
    PenUp _cid' pcoord -> do 
      let zmode = get (zoomMode.viewInfo) cvsInfo
          (x,y) = device2pageCoord cpg zmode pcoord 
      disconnect connidmove
      disconnect connidup
      return (pdraw |> (x,y)) 
    _ -> do
      penProcess cid cpg connidmove connidup pdraw (x0,y0) 
