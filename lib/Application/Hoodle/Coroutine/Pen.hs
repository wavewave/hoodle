{-# LANGUAGE Rank2Types, GADTs, ScopedTypeVariables, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.Hoodle.Coroutine.Pen 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.Hoodle.Coroutine.Pen where

import Graphics.UI.Gtk hiding (get,set,disconnect)
import Application.Hoodle.Device 
import Application.Hoodle.Type.Event
import Application.Hoodle.Type.Enum
import Application.Hoodle.Type.Coroutine
import Application.Hoodle.Type.Canvas
import Application.Hoodle.Type.PageArrangement
import Application.Hoodle.Type.XournalState
import Application.Hoodle.Coroutine.Draw
import Application.Hoodle.Coroutine.EventConnect
import Application.Hoodle.Coroutine.Commit
import Application.Hoodle.Accessor
import Application.Hoodle.ModelAction.Pen
import Application.Hoodle.ModelAction.Page
import Application.Hoodle.View.Coordinate
import Application.Hoodle.View.Draw
import Application.Hoodle.Util
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Xournal.Predefined
import Data.Xournal.BBox
import Data.Sequence hiding (filter)
import qualified Data.Map as M
import Data.Maybe 
import Control.Category
import Data.Label
import Prelude hiding ((.), id)


-- | page switch if pen click a page different than the current page

penPageSwitch :: (ViewMode a) => 
                 CanvasInfo a -> PageNum -> MainCoroutine (CanvasInfo a)
penPageSwitch cinfo pgn = do (xst,cinfo') <- getSt >>= switchact 
                             putSt xst     
                             return cinfo'
  where switchact xst = do 
          let ncinfo = set currentPageNum (unPageNum pgn) cinfo 
              mfunc = const (return . CanvasInfoBox $ ncinfo)  
          return . (,ncinfo) =<< modifyCurrCvsInfoM mfunc xst


-- | Common Pen Work starting point 

commonPenStart :: (forall a. ViewMode a => CanvasInfo a -> PageNum -> CanvasGeometry  
                    -> (ConnectId DrawingArea, ConnectId DrawingArea) 
                    -> (Double,Double) -> MainCoroutine () )
               -> CanvasId -> PointerCoord 
               -> MainCoroutine ()
commonPenStart action cid pcoord = do
    oxstate <- getSt 
    let currcid = getCurrentCanvasId oxstate
    when (cid /= currcid) (changeCurrentCanvasId cid >> invalidateAll)
    nxstate <- getSt
    boxAction f . getCanvasInfo cid $ nxstate
  where f :: forall b. (ViewMode b) => CanvasInfo b -> MainCoroutine ()
        f cvsInfo = do 
          let cpn = PageNum . get currentPageNum $ cvsInfo
              arr = get (pageArrangement.viewInfo) cvsInfo              
              canvas = get drawArea cvsInfo
          geometry <- liftIO $ makeCanvasGeometry cpn arr canvas
          let pagecoord = desktop2Page geometry . device2Desktop geometry $ pcoord 
          maybeFlip pagecoord (return ()) 
            $ \(pgn,PageCoord (x,y)) -> do 
                 nCvsInfo <- if (cpn /= pgn) 
                               then penPageSwitch cvsInfo pgn
                               else return cvsInfo                   
                 connidup   <- connectPenUp nCvsInfo 
                 connidmove <- connectPenMove nCvsInfo
                 action nCvsInfo pgn geometry (connidup,connidmove) (x,y) 

      
-- | enter pen drawing mode

penStart :: CanvasId -> PointerCoord -> MainCoroutine () 
penStart cid pcoord = commonPenStart penAction cid pcoord
  where penAction :: forall b. (ViewMode b) => CanvasInfo b -> PageNum -> CanvasGeometry -> (ConnectId DrawingArea, ConnectId DrawingArea) -> (Double,Double) -> MainCoroutine ()
        penAction _cinfo pnum geometry (cidmove,cidup) (x,y) = do 
          xstate <- getSt
          let PointerCoord _ _ _ z = pcoord 
          let currxoj = unView . get xournalstate $ xstate        
              pinfo = get penInfo xstate
          pdraw <-penProcess cid pnum geometry cidmove cidup (empty |> (x,y,z)) ((x,y),z) 
          (newxoj,bbox) <- liftIO $ addPDraw pinfo currxoj pnum pdraw
          commit . set xournalstate (ViewAppendState newxoj) 
                 =<< (liftIO (updatePageAll (ViewAppendState newxoj) xstate))
          let f = unDeskCoord . page2Desktop geometry . (pnum,) . PageCoord
              nbbox = xformBBox f bbox 
          invalidateAllInBBox (Just (inflate nbbox 2.0))

-- | main pen coordinate adding process
-- | now being changed

penProcess :: CanvasId -> PageNum 
           -> CanvasGeometry
           -> ConnectId DrawingArea -> ConnectId DrawingArea 
           -> Seq (Double,Double,Double) -> ((Double,Double),Double) 
           -> MainCoroutine (Seq (Double,Double,Double))
penProcess cid pnum geometry connidmove connidup pdraw ((x0,y0),z0) = do 
    r <- await 
    xst <- getSt 
    boxAction (fsingle r xst) . getCanvasInfo cid $ xst
  where 
    fsingle :: forall b. (ViewMode b) => 
               MyEvent -> HoodleState -> CanvasInfo b 
               -> MainCoroutine (Seq (Double,Double,Double))
    fsingle r xstate cvsInfo = 
      penMoveAndUpOnly r pnum geometry 
        (penProcess cid pnum geometry connidmove connidup pdraw ((x0,y0),z0))
        (\(pcoord,(x,y)) -> do 
           let PointerCoord _ _ _ z = pcoord 
           let canvas = get drawArea cvsInfo
               ptype  = get (penType.penInfo) xstate
               pcolor = get (penColor.currentTool.penInfo) xstate 
               pwidth = get (penWidth.currentTool.penInfo) xstate 
               (pcr,pcg,pcb,pca)= fromJust (M.lookup pcolor penColorRGBAmap) 
               opacity = case ptype of 
                  HighlighterWork -> predefined_highlighter_opacity 
                  _ -> 1.0
               pcolRGBA = (pcr,pcg,pcb,pca*opacity) 
           let pressureType = case get (variableWidthPen.penInfo) xstate of 
                                True -> Pressure
                                False -> NoPressure
           liftIO $ drawCurvebitGen pressureType canvas geometry 
                      pwidth pcolRGBA pnum ((x0,y0),z0) ((x,y),z)
           penProcess cid pnum geometry connidmove connidup (pdraw |> (x,y,z)) ((x,y),z) )
        (\_ -> disconnect connidmove >> disconnect connidup >> return pdraw )

-- | 
    
skipIfNotInSamePage :: Monad m => 
                       PageNum -> CanvasGeometry -> PointerCoord 
                       -> m a 
                       -> ((PointerCoord,(Double,Double)) -> m a)
                       -> m a
skipIfNotInSamePage  pgn geometry pcoord skipaction ordaction =  
  switchActionEnteringDiffPage pgn geometry pcoord 
    skipaction (\_ _ -> skipaction ) (\_ (_,PageCoord xy)->ordaction (pcoord,xy)) 
  
-- |       

switchActionEnteringDiffPage :: Monad m => 
                                PageNum -> CanvasGeometry -> PointerCoord 
                                -> m a 
                                -> (PageNum -> (PageNum,PageCoordinate) -> m a)
                                -> (PageNum -> (PageNum,PageCoordinate) -> m a)
                                -> m a
switchActionEnteringDiffPage pgn geometry pcoord skipaction chgaction ordaction = do 
    let pagecoord = desktop2Page geometry . device2Desktop geometry $ pcoord 
    maybeFlip pagecoord skipaction 
      $ \(cpn, pxy) -> if pgn == cpn 
                       then ordaction pgn (cpn,pxy) 
                       else chgaction pgn (cpn,pxy)
                                                                 
        
-- | in page action  
    
penMoveAndUpOnly :: Monad m => MyEvent 
                    -> PageNum 
                    -> CanvasGeometry 
                    -> m a 
                    -> ((PointerCoord,(Double,Double)) -> m a) 
                    -> (PointerCoord -> m a) 
                    -> m a
penMoveAndUpOnly r pgn geometry defact moveaction upaction = 
  case r of 
    PenMove _ pcoord -> skipIfNotInSamePage pgn geometry pcoord defact moveaction  
    PenUp _ pcoord -> upaction pcoord  
    _ -> defact 
  
penMoveAndUpInterPage :: Monad m => MyEvent 
                      -> PageNum 
                      -> CanvasGeometry 
                      -> m a 
                      -> (PageNum -> (PageNum,PageCoordinate) -> m a) 
                      -> (PointerCoord -> m a) 
                      -> m a
penMoveAndUpInterPage r pgn geometry defact moveaction upaction = 
  case r of 
    PenMove _ pcoord -> 
      switchActionEnteringDiffPage pgn geometry pcoord defact moveaction moveaction  
    PenUp _ pcoord -> upaction pcoord  
    _ -> defact 
  



