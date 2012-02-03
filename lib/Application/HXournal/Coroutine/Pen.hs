{-# LANGUAGE Rank2Types, GADTs, ScopedTypeVariables, TupleSections #-}

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
-----------------------------------------------------------------------------

module Application.HXournal.Coroutine.Pen where

import Graphics.UI.Gtk hiding (get,set,disconnect)
import Application.HXournal.Device 
import Application.HXournal.Type.Event
import Application.HXournal.Type.Enum
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.PageArrangement
import Application.HXournal.Type.XournalState
import Application.HXournal.Coroutine.Draw
import Application.HXournal.Coroutine.EventConnect
import Application.HXournal.Coroutine.Commit
import Application.HXournal.Accessor
import Application.HXournal.ModelAction.Pen
import Application.HXournal.ModelAction.Page
import Application.HXournal.View.Coordinate
import Application.HXournal.View.Draw
import Application.HXournal.Type.Alias
import Application.HXournal.Util
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Xournal.Predefined
import Data.Xournal.Generic
import Data.Xournal.BBox
import Graphics.Xournal.Render.BBox
import Data.Sequence hiding (filter)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Maybe 
import Control.Category
import Data.Label
import Prelude hiding ((.), id)

-- | page switch if pen click a page different than the current page

penPageSwitch :: PageNum -> MainCoroutine ()
penPageSwitch pgn = updateXState switchact
  where switchact xst = do 
          liftIO $ putStrLn "penPageSwitch"
          let xoj = getXournal xst
          let page = maybeError "no such page in penPageSwitch" 
                      $ IM.lookup (unPageNum pgn) (get g_pages xoj)
              mfunc (CanvasInfoBox cinfo) = do
                 return $ CanvasInfoBox $ set currentPageNum (unPageNum pgn)
                                          . set currentPage (Left page) 
                                          $ cinfo 
          modifyCurrCvsInfoM mfunc xst



-- | Common Pen Work starting point 

commonPenStart :: (forall a. ViewMode a => CanvasInfo a -> PageNum -> CanvasGeometry  
                    -> (ConnectId DrawingArea, ConnectId DrawingArea) 
                    -> (Double,Double) -> MainCoroutine () )
               -> CanvasId -> PointerCoord 
               -> MainCoroutine ()
commonPenStart action cid pcoord =
    boxAction f . getCanvasInfo cid =<< changeCurrentCanvasId cid 
  where f :: forall b. (ViewMode b) => CanvasInfo b -> MainCoroutine ()
        f cvsInfo = do 
          let page = getPage cvsInfo
              cpn = PageNum . get currentPageNum $ cvsInfo
              arr = get (pageArrangement.viewInfo) cvsInfo              
              canvas = get drawArea cvsInfo
          geometry <- liftIO $ makeCanvasGeometry EditMode (cpn,page) arr canvas
          let pagecoord = desktop2Page geometry . device2Desktop geometry $ pcoord 
          maybeFlip pagecoord (return ()) 
            $ \(pgn,PageCoord (x,y)) -> do 
                 liftIO $ putStrLn $ show (pgn,(x,y))
                 when (cpn /= pgn) (penPageSwitch pgn)
                 connidup   <- connectPenUp cvsInfo 
                 connidmove <- connectPenMove cvsInfo
                 action cvsInfo pgn geometry (connidup,connidmove) (x,y) 

      
-- | enter pen drawing mode

penStart :: CanvasId -> PointerCoord -> MainCoroutine () 
penStart cid = commonPenStart penAction cid 
  where penAction :: forall b. (ViewMode b) => CanvasInfo b -> PageNum -> CanvasGeometry -> (ConnectId DrawingArea, ConnectId DrawingArea) -> (Double,Double) -> MainCoroutine ()
        penAction cinfo pnum geometry (cidmove,cidup) (x,y) = do 
          xstate <- getSt
          let currxoj = unView . get xournalstate $ xstate        
              pinfo = get penInfo xstate
          pdraw <-penProcess cid pnum geometry cidmove cidup (empty |> (x,y)) (x,y) 
          (newxoj,bbox) <- liftIO $ addPDraw pinfo currxoj pnum pdraw
          commit . set xournalstate (ViewAppendState newxoj) 
                 =<< (liftIO (updatePageAll (ViewAppendState newxoj) xstate))
            
          let f = unDeskCoord . page2Desktop geometry . (pnum,) . PageCoord
              nbbox = xformBBox f bbox 
          invalidateAll
          -- invalidateAllInBBox (Just (inflate bbox 2.0))



-- | main pen coordinate adding process

-- | now being changed

penProcess :: CanvasId -> PageNum 
           -> CanvasGeometry
           -> ConnectId DrawingArea -> ConnectId DrawingArea 
           -> Seq (Double,Double) -> (Double,Double) 
           -> MainCoroutine (Seq (Double,Double))
penProcess cid pnum geometry connidmove connidup pdraw (x0,y0) = do 
    r <- await 
    xst <- getSt 
    selectBoxAction (fsingle r xst) (fsingle r xst) . getCanvasInfo cid $ xst
  where 
    fsingle :: forall b. (ViewMode b) => 
               MyEvent -> HXournalState -> CanvasInfo b -> MainCoroutine (Seq (Double,Double))
    fsingle r xstate cvsInfo = 
      penMoveAndUpOnly r pnum geometry 
        (penProcess cid pnum geometry connidmove connidup pdraw (x0,y0))
        (\(x,y) -> do 
           let canvas = get drawArea cvsInfo
               ptype  = get (penType.penInfo) xstate
               pcolor = get (penColor.currentTool.penInfo) xstate 
               pwidth = get (penWidth.currentTool.penInfo) xstate 
               (pcr,pcg,pcb,pca)= fromJust (M.lookup pcolor penColorRGBAmap) 
               opacity = case ptype of 
                  HighlighterWork -> predefined_highlighter_opacity 
                  _ -> 1.0
               pcolRGBA = (pcr,pcg,pcb,pca*opacity) 
           liftIO $ drawCurvebit canvas geometry pwidth pcolRGBA pnum (x0,y0) (x,y)
           penProcess cid pnum geometry connidmove connidup (pdraw |> (x,y)) (x,y) )
        (\_ -> disconnect connidmove >> disconnect connidup >> return pdraw )

      
skipIfNotInSamePage :: Monad m => 
                       PageNum -> CanvasGeometry -> PointerCoord 
                       -> m a -> ((Double,Double) -> m a) -> m a
skipIfNotInSamePage pgn geometry pcoord skipaction realaction = do 
    let pagecoord = desktop2Page geometry . device2Desktop geometry $ pcoord 
   
    maybeFlip pagecoord skipaction 
      $ \(cpn, PageCoord (x,y)) -> if pgn == cpn then realaction (x,y) else skipaction
                                                                 
        

penMoveAndUpOnly :: Monad m => MyEvent 
                    -> PageNum 
                    -> CanvasGeometry 
                    -> m a 
                    -> ((Double,Double) -> m a) 
                    -> (PointerCoord -> m a) 
                    -> m a
penMoveAndUpOnly r pgn geometry defact moveaction upaction = 
  case r of 
    PenMove _ pcoord -> skipIfNotInSamePage pgn geometry pcoord defact moveaction  
    PenUp _ pcoord -> upaction pcoord  
    _ -> defact 
  



