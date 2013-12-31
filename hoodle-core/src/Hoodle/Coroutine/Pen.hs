{-# LANGUAGE Rank2Types, GADTs, ScopedTypeVariables, TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Pen 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Pen where

-- from other packages
import           Control.Applicative ((<$>),(<*>))
import           Control.Lens (view,set,at)
import           Control.Monad hiding (mapM_,forM_)
import           Control.Monad.State hiding (mapM_,forM_)
-- import Control.Monad.Trans
import           Data.Functor.Identity (Identity(..))
import           Data.Foldable (forM_)
import           Data.Sequence hiding (filter)
import           Data.Maybe 
import           Data.Time.Clock 
import           Graphics.Rendering.Cairo
-- from hoodle-platform
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic (gpages)
import           Data.Hoodle.Simple (Dimension(..))
import           Graphics.Hoodle.Render (renderStrk)
-- from this package
import           Hoodle.Accessor
import           Hoodle.Device 
import           Hoodle.Coroutine.Commit
import           Hoodle.Coroutine.Draw
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Pen
import           Hoodle.Type.Alias
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.Predefined 
import           Hoodle.Type.HoodleState
import           Hoodle.Util
import           Hoodle.View.Coordinate
import           Hoodle.View.Draw
--
import Prelude hiding (mapM_)



-- |
createTempRender :: CanvasGeometry -> a -> MainCoroutine (TempRender a) 
createTempRender geometry x = do 
    xst <- get
    let cinfobox = view currentCanvasInfo xst 
        mcvssfc = view (unboxLens mDrawSurface) cinfobox 
    let hdl = getHoodle xst
    let Dim cw ch = unCanvasDimension . canvasDim $ geometry
        
    srcsfc <- liftIO  $ maybe (fst <$> canvasImageSurface Nothing geometry hdl)
                              (\cvssfc -> do 
                                sfc <- createImageSurface FormatARGB32 (floor cw) (floor ch) 
                                renderWith sfc $ do 
                                  setSourceSurface cvssfc 0 0 
                                  setOperator OperatorSource 
                                  paint
                                return sfc) 
                              mcvssfc
    liftIO $ renderWith srcsfc $ do 
      emphasisCanvasRender ColorRed geometry 
    tgtsfc <- liftIO $ createImageSurface FormatARGB32 (floor cw) (floor ch) 
    let trdr = TempRender srcsfc tgtsfc (cw,ch) x
    return trdr 

-- | page switch if pen click a page different than the current page
penPageSwitch :: PageNum -> MainCoroutine CanvasInfoBox 
penPageSwitch pgn = do 
    xstate <- get
    let cibox = view currentCanvasInfo xstate     
        ncibox = (runIdentity . forBoth unboxBiXform (return . set currentPageNum (unPageNum pgn))) cibox 
    put (set currentCanvasInfo ncibox xstate) 
    invalidateAllInBBox Nothing Efficient
    return ncibox 
        

-- | Common Pen Work starting point 
commonPenStart :: (forall a. CanvasInfo a -> PageNum -> CanvasGeometry  
                    -> (Double,Double) -> MainCoroutine () )
               -> CanvasId -> PointerCoord 
               -> MainCoroutine ()
commonPenStart action cid pcoord = do
    oxstate <- get 
    let currcid = getCurrentCanvasId oxstate
    when (cid /= currcid) (changeCurrentCanvasId cid >> invalidateAll)
    nxstate <- get
    forBoth' unboxBiAct f . getCanvasInfo cid $ nxstate
  where f :: forall b. CanvasInfo b -> MainCoroutine ()
        f cvsInfo = do 
          let cpn = PageNum . view currentPageNum $ cvsInfo
              arr = view (viewInfo.pageArrangement) cvsInfo 
              canvas = view drawArea cvsInfo
          geometry <- liftIO $ makeCanvasGeometry cpn arr canvas
          let pagecoord = desktop2Page geometry . device2Desktop geometry $ pcoord 
          maybeFlip pagecoord (return ()) 
            $ \(pgn,PageCoord (x,y)) -> do 
                 nCvsInfo <- if (cpn /= pgn) 
                               then do penPageSwitch pgn
                                    -- temporary dirty fix 
                                       return (set currentPageNum (unPageNum pgn) cvsInfo )
                               else return cvsInfo                   
                 action nCvsInfo pgn geometry (x,y) 
      
-- | enter pen drawing mode
penStart :: CanvasId -> PointerCoord -> MainCoroutine () 
penStart cid pcoord = commonPenStart penAction cid pcoord
  where penAction :: forall b. CanvasInfo b -> PageNum -> CanvasGeometry -> (Double,Double) -> MainCoroutine ()
        penAction _cinfo pnum geometry (x,y) = do 
          xstate <- get
          let PointerCoord _ _ _ z = pcoord 
          let currhdl = getHoodle  xstate 
              pinfo = view penInfo xstate
              mpage = view (gpages . at (unPageNum pnum)) currhdl 
          forM_ mpage $ \page -> do 
            trdr <- createTempRender geometry (empty |> (x,y,z)) 
            pdraw <-penProcess cid pnum geometry trdr ((x,y),z) 
            surfaceFinish (tempSurfaceSrc trdr)
            surfaceFinish (tempSurfaceTgt trdr)            
            --
            case viewl pdraw of 
              EmptyL -> return ()
              (x1,_y1,_z1) :< _rest -> do 
                if x1 <= 1e-3      -- this is ad hoc but.. 
                  then invalidateAll
                  else do  
                    (newhdl,bbox) <- liftIO $ addPDraw pinfo currhdl pnum pdraw
                    commit . set hoodleModeState (ViewAppendState newhdl) 
                      =<< (liftIO (updatePageAll (ViewAppendState newhdl) xstate))
                    let f = unDeskCoord . page2Desktop geometry . (pnum,) . PageCoord
                        nbbox = xformBBox f bbox 
                    invalidateAllInBBox (Just nbbox) BkgEfficient 
          
-- | main pen coordinate adding process
-- | now being changed
penProcess :: CanvasId -> PageNum 
           -> CanvasGeometry
           -> TempRender (Seq (Double,Double,Double))
           -> ((Double,Double),Double) 
           -> MainCoroutine (Seq (Double,Double,Double))
penProcess cid pnum geometry trdr ((x0,y0),z0) = do 
    r <- nextevent
    xst <- get 
    forBoth' unboxBiAct (fsingle r xst) . getCanvasInfo cid $ xst
  where 
    pdraw = tempInfo trdr 
    fsingle :: forall b.  
               UserEvent -> HoodleState -> CanvasInfo b 
               -> MainCoroutine (Seq (Double,Double,Double))
    fsingle r xstate cvsInfo = 
      penMoveAndUpOnly r pnum geometry 
        (penProcess cid pnum geometry trdr ((x0,y0),z0))
        (\(pcoord,(x,y)) -> do 
           let PointerCoord _ _ _ z = pcoord 
           let pinfo  = view penInfo xstate
           let xformfunc = cairoXform4PageCoordinate geometry pnum 
               tmpstrk = createNewStroke pinfo pdraw
               renderfunc = do 
                 xformfunc 
                 renderStrk tmpstrk
           let (srcsfc,tgtsfc) = (,) <$> tempSurfaceSrc <*> tempSurfaceTgt $ trdr
           virtualDoubleBufferDraw srcsfc tgtsfc (return ()) renderfunc
           liftIO $ doubleBufferFlush tgtsfc cvsInfo
           ---                                
           let ntrdr = trdr { tempInfo = pdraw |> (x,y,z) }
           penProcess cid pnum geometry ntrdr ((x,y),z) )
        (\_ -> return pdraw )

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
penMoveAndUpOnly :: Monad m => UserEvent 
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
  
-- | 
penMoveAndUpInterPage :: Monad m => UserEvent 
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
  
-- | process action when last time was before time diff limit, otherwise
--   just do default action.
processWithTimeInterval :: (Monad m, MonadIO m) =>         
                           NominalDiffTime   -- ^ time diff
                        -> (UTCTime -> m a)  -- ^ not larger than time diff bound
                        -> (UTCTime -> m a)  -- ^ larger than time diff bound 
                        -> UTCTime           -- ^ last updated time
                        -> m a
processWithTimeInterval tdiffbound defact updateact otime = do  
    ctime <- liftIO getCurrentTime 
    let dtime = diffUTCTime ctime otime 
    if dtime > tdiffbound then updateact ctime else defact otime 

-- |
processWithDefTimeInterval :: (Monad m, MonadIO m) =>         
                                 (UTCTime -> m a)  -- ^ not larger than time diff bound
                              -> (UTCTime -> m a)  -- ^ larger than time diff bound 
                              -> UTCTime           -- ^ last updated time
                              -> m a
processWithDefTimeInterval = processWithTimeInterval dtime_bound 

                   



