{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Pen 
-- Copyright   : (c) 2011-2016 Ian-Woo Kim
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
import           Control.Lens (at,over,set,view)
import           Control.Monad hiding (mapM_,forM_)
import           Control.Monad.State hiding (mapM_,forM_)
-- import Control.Monad.Trans
import           Data.Functor.Identity (Identity(..))
import           Data.Foldable (toList)
import           Data.Sequence hiding (filter)
import qualified Data.IntMap as IM
import           Data.Maybe 
import           Data.Ratio
import           Data.Time.Clock 
import qualified Graphics.Rendering.Cairo as Cairo
-- from hoodle-platform
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic (gitems,gpages)
import           Data.Hoodle.Simple (Dimension(..))
import           Graphics.Hoodle.Render (renderStrk,updateLayerBuf)
import           Graphics.Hoodle.Render.Type
-- from this package
import           Hoodle.Accessor
import           Hoodle.Coroutine.Commit
import           Hoodle.Coroutine.Draw
import           Hoodle.Device 
import           Hoodle.GUI.Reflect
import           Hoodle.ModelAction.Layer
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Pen
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
import           Prelude hiding (mapM_)


-- | 
addPDraw :: CanvasId
         -> PenInfo 
         -> RHoodle
         -> PageNum 
         -> Seq (Double,Double,Double) 
         -> MainCoroutine (RHoodle,BBox) -- ^ new hoodle and bbox in page coordinate
addPDraw cid pinfo hdl (PageNum pgnum) pdraw = do 
    let currpage = getPageFromGHoodleMap pgnum hdl
        currlayer = getCurrentLayer currpage
        newstroke = createNewStroke pinfo pdraw         
        newstrokebbox = runIdentity (makeBBoxed newstroke)
        bbox = getBBox newstrokebbox
        newlayerbbox = over gitems (++[RItemStroke newstrokebbox]) currlayer
    callRenderer_ $ updateLayerBuf cid newlayerbbox
    let newpagebbox = adjustCurrentLayer newlayerbbox currpage 
        newhdlbbox = set gpages (IM.adjust (const newpagebbox) pgnum (view gpages hdl) ) hdl 
    return (newhdlbbox,bbox)


-- |
createTempRender :: CanvasGeometry -> a -> MainCoroutine (TempRender a) 
createTempRender geometry x = do 
    xst <- get
    cache <- renderCache
    let uhdl = view (unitHoodles.currentUnit) xst
        cinfobox = view currentCanvasInfo uhdl
        mcvssfc = view (unboxLens mDrawSurface) cinfobox 
        cid = getCurrentCanvasId uhdl
        hdl = getHoodle uhdl
        Dim cw ch = unCanvasDimension . canvasDim $ geometry
    srcsfc <- liftIO $  
      maybe (fst <$> canvasImageSurface cache cid Nothing geometry hdl)
            (\cvssfc -> do 
              sfc <- Cairo.createImageSurface 
                       Cairo.FormatARGB32 (floor cw) (floor ch)
              Cairo.renderWith sfc $ do
                Cairo.setSourceSurface cvssfc 0 0 
                Cairo.setOperator Cairo.OperatorSource 
                Cairo.paint
                Cairo.setSourceRGBA 1 0 0 1
                Cairo.rectangle 100 100 200 200
                Cairo.fill

              return sfc) 
            mcvssfc
    liftIO $ Cairo.renderWith srcsfc $ do 
      emphasisCanvasRender ColorRed geometry 
    tgtsfc <- liftIO (Cairo.createImageSurface Cairo.FormatARGB32 (floor cw) (floor ch))
    let trdr = TempRender srcsfc tgtsfc (cw,ch) x
    return trdr 

-- | page switch if pen click a page different than the current page
penPageSwitch :: PageNum -> MainCoroutine CanvasInfoBox 
penPageSwitch pgn = do 
    xstate <- get
    let uhdl = view (unitHoodles.currentUnit) xstate
        cibox = view currentCanvasInfo uhdl    
        ncibox = (runIdentity . forBoth unboxBiXform (return . set currentPageNum (unPageNum pgn))) cibox 
        uhdl' = set currentCanvasInfo ncibox uhdl
    pureUpdateUhdl (const uhdl')
    invalidateAllInBBox Nothing Efficient
    return ncibox 
       
-- | Common Pen Work starting point 
commonPenStart :: forall b. 
                  (forall a . CanvasInfo a -> PageNum -> CanvasGeometry  
                    -> (Double,Double) -> UTCTime -> MainCoroutine b)
               -> CanvasId 
               -> PointerCoord 
               -> MainCoroutine (Maybe b)
commonPenStart action cid pcoord = do
    oxstate <- get 
    let currcid = (getCurrentCanvasId . view (unitHoodles.currentUnit)) oxstate
    ctime <- liftIO $ getCurrentTime
    when (cid /= currcid) (changeCurrentCanvasId cid >> invalidateAll)
    nxstate <- get
    forBoth' unboxBiAct (f ctime) . getCanvasInfo cid . view (unitHoodles.currentUnit) $ nxstate
  where f :: forall c. UTCTime -> CanvasInfo c -> MainCoroutine (Maybe b)
        f ctime cvsInfo = do 
          let cpn = PageNum . view currentPageNum $ cvsInfo
              arr = view (viewInfo.pageArrangement) cvsInfo 
              canvas = view drawArea cvsInfo
          geometry <- liftIO $ makeCanvasGeometry cpn arr canvas
          let pagecoord = desktop2Page geometry . device2Desktop geometry $ pcoord 
          maybeFlip pagecoord (return Nothing) 
            $ \(pgn,PageCoord (x,y)) -> do 
                 nCvsInfo <- if (cpn /= pgn) 
                               then do penPageSwitch pgn
                                    -- temporary dirty fix 
                                       return (set currentPageNum (unPageNum pgn) cvsInfo )
                               else return cvsInfo                   
                 Just <$> action nCvsInfo pgn geometry (x,y) ctime 
      
-- | enter pen drawing mode
penStart :: CanvasId 
         -> PointerCoord 
         -> MainCoroutine (Maybe (Maybe (Maybe ())))
penStart cid pcoord = commonPenStart penAction cid pcoord
  where penAction :: forall b. CanvasInfo b -> PageNum -> CanvasGeometry 
                  -> (Double,Double) -> UTCTime 
                  -> MainCoroutine (Maybe (Maybe ()))
        penAction _cinfo pnum geometry (x,y) ctime = do
          xstate <- get
          let uhdl = view (unitHoodles.currentUnit) xstate
          let PointerCoord _ _ _ z = pcoord 
          let currhdl = getHoodle  uhdl
              pinfo = view penInfo xstate
              mpage = view (gpages . at (unPageNum pnum)) currhdl 
          maybeFlip mpage (return Nothing)  $ \_page -> do 
            trdr <- createTempRender geometry (empty |> (x,y,z)) 
            mpdraw <-penProcess cid pnum geometry trdr ((x,y),z) ctime
            Cairo.surfaceFinish (tempSurfaceSrc trdr)
            Cairo.surfaceFinish (tempSurfaceTgt trdr)
            maybeFlip mpdraw (return (Just Nothing)) $ \pdraw -> 
              case viewl pdraw of 
                EmptyL -> return (Just (Just ()))
                (x1,_y1,_z1) :< _rest -> do 
                  if x1 <= 1e-3      -- this is ad hoc but.. 
                    then invalidateAll
                    else do  
                      (newhdl,_bbox) <- addPDraw cid pinfo currhdl pnum pdraw
                      uhdl' <- liftIO (updatePageAll (ViewAppendState newhdl) uhdl)
                      commit (set (unitHoodles.currentUnit) uhdl' xstate)
                      return ()
                  return (Just (Just ()))
          
-- | main pen coordinate adding process
-- | now being changed
penProcess :: CanvasId 
           -> PageNum 
           -> CanvasGeometry
           -> TempRender (Seq (Double,Double,Double))
           -> ((Double,Double),Double) 
           -> UTCTime
           -> MainCoroutine (Maybe (Seq (Double,Double,Double)))
penProcess cid pnum geometry trdr ((x0,y0),z0) ctime = do 
    r <- nextevent
    ntime <- liftIO getCurrentTime
    let ispressandhold = 
          abs (toRational (diffUTCTime ctime ntime)) > 1 % 2
        lst = (toList . tempInfo) trdr
        deltax = let xlst = map (\(x,_,_)->x) lst
                 in abs (maximum xlst - minimum xlst)
        deltay = let ylst = map (\(_,y,_)->y) lst
                 in abs (maximum ylst - minimum ylst)
    -- temporarily fix the range
    if (deltax < 20 && deltay < 20 && ispressandhold && Prelude.length lst < 20) 
      then return Nothing 
      else do xst <- get 
              forBoth' unboxBiAct (f r xst) . (getCanvasInfo cid . view (unitHoodles.currentUnit)) $ xst
  where 
    pdraw = tempInfo trdr 
    f :: forall b.  
         UserEvent -> HoodleState -> CanvasInfo b  
      -> MainCoroutine (Maybe (Seq (Double,Double,Double)))
    f r xstate cvsInfo = 
      penMoveAndUpOnly r pnum geometry 
        (penProcess cid pnum geometry trdr ((x0,y0),z0) ctime)
        (\(pcoord,(x,y)) -> do 
           liftIO $ print (x,y)
           let PointerCoord _ _ _ z = pcoord 
           let pinfo  = view penInfo xstate
           let xformfunc = cairoXform4PageCoordinate (mkXform4Page geometry pnum )
               tmpstrk = createNewStroke pinfo pdraw
               renderfunc = do 
                 xformfunc 
                 renderStrk tmpstrk
           let (srcsfc,tgtsfc) = (,) <$> tempSurfaceSrc <*> tempSurfaceTgt $ trdr
           -- virtualDoubleBufferDraw srcsfc tgtsfc (return ()) renderfunc
           liftIO $ doubleBufferFlush tgtsfc cvsInfo
           ---                                
           let ntrdr = trdr { tempInfo = pdraw |> (x,y,z) }
           penProcess cid pnum geometry ntrdr ((x,y),z) ctime)
        (\_ -> return (Just pdraw))

-- | 
skipIfNotInSamePage :: Monad m => 
                       PageNum 
                    -> CanvasGeometry 
                    -> PointerCoord 
                    -> m a 
                    -> ((PointerCoord,(Double,Double)) -> m a)
                    -> m a
skipIfNotInSamePage  pgn geometry pcoord skipaction ordaction =  
  switchActionEnteringDiffPage pgn geometry pcoord 
    skipaction (\_ _ -> skipaction ) (\_ (_,PageCoord xy)->ordaction (pcoord,xy)) 
  
-- |       
switchActionEnteringDiffPage :: Monad m => 
                                PageNum 
                             -> CanvasGeometry 
                             -> PointerCoord 
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
penMoveAndUpOnly :: Monad m => 
                    UserEvent 
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
penMoveAndUpInterPage :: Monad m => 
                         UserEvent 
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

