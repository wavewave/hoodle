{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Draw 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Draw where

-- from other packages
import           Control.Applicative
import           Control.Lens (view,set,(^.),(%~),(.~))
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Reader (runReaderT)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as M
import           Data.Time.Clock
import           Data.Time.LocalTime
import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.UI.Gtk hiding (get,set)
-- from hoodle-platform
import           Control.Monad.Trans.Crtn
import           Control.Monad.Trans.Crtn.Object
import           Control.Monad.Trans.Crtn.Queue
import           Data.Hoodle.BBox
import           Graphics.Hoodle.Render.Type
-- import           Graphics.Hoodle.Render.Type.Renderer
-- from this package
import           Hoodle.Accessor
import           Hoodle.Type.Alias
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.HoodleState
import           Hoodle.Type.Widget
import           Hoodle.View.Draw
-- import           Hoodle.Widget.Clock
-- 


-- | all event
nextevent :: MainCoroutine UserEvent 
nextevent = do Arg DoEvent ev <- request (Res DoEvent ())
               case ev of
                 SysEv sev -> sysevent sev >> nextevent 
                 UsrEv uev -> return uev 

-- | system event 
sysevent :: SystemEvent -> MainCoroutine () 
sysevent ClockUpdateEvent = do 
  utctime <- liftIO $ getCurrentTime 
  zone <- liftIO $ getCurrentTimeZone  
  let ltime = utcToLocalTime zone utctime 
      ltimeofday = localTimeOfDay ltime 
      (h,m,s) :: (Int,Int,Int) = 
        (,,) <$> (\x->todHour x `mod` 12) <*> todMin <*> (floor . todSec) 
        $ ltimeofday
  xst <- get 
  let uhdl = view (unitHoodles.currentUnit) xst
      cinfo = view currentCanvasInfo uhdl
      cwgts = view (unboxLens canvasWidgets) cinfo   
      nwgts = set (clockWidgetConfig.clockWidgetTime) (h,m,s) cwgts
      ncinfo = set (unboxLens canvasWidgets) nwgts cinfo
  -- modify (set unitHoodles ((putTheUnit . set currentCanvasInfo ncinfo) uhdl))
  pureUpdateUhdl (const ((currentCanvasInfo .~ ncinfo) uhdl))
  when (view (widgetConfig.doesUseClockWidget) cwgts) $ do 
    let cid = getCurrentCanvasId uhdl
    modify (tempQueue %~ enqueue (Right (UsrEv (UpdateCanvasEfficient cid))))
sysevent (RenderCacheUpdate (uuid, ssfc)) = do
  modify (renderCache %~ HM.insert uuid ssfc)
  b <- ( ^. doesNotInvalidate ) <$> get
  when (not b) $ invalidateAll
sysevent ev = liftIO $ print ev 


-- |
data DrawingFunctionSet = 
  DrawingFunctionSet { singleEditDraw :: DrawingFunction SinglePage EditMode
                     , singleSelectDraw :: DrawingFunction SinglePage SelectMode
                     , contEditDraw :: DrawingFunction ContinuousPage EditMode
                     , contSelectDraw :: DrawingFunction ContinuousPage SelectMode 
                     }

-- | 
invalidateGeneral :: CanvasId -> Maybe BBox -> DrawFlag 
                  -> DrawingFunction SinglePage EditMode
                  -> DrawingFunction SinglePage SelectMode
                  -> DrawingFunction ContinuousPage EditMode
                  -> DrawingFunction ContinuousPage SelectMode
                  -> MainCoroutine () 
invalidateGeneral cid mbbox flag drawf drawfsel drawcont drawcontsel = do 
    xst <- get
    let uhdl = view (unitHoodles.currentUnit) xst
        cache = view renderCache xst
    unboxBiAct (fsingle cache uhdl) (fcont cache uhdl) . getCanvasInfo cid $ uhdl
  where 
    fsingle :: RenderCache -> UnitHoodle -> CanvasInfo SinglePage -> MainCoroutine () 
    fsingle cache uhdl cvsInfo = do 
      let cpn = PageNum . view currentPageNum $ cvsInfo 
	  isCurrentCvs = cid == getCurrentCanvasId uhdl
	  epage = getCurrentPageEitherFromHoodleModeState cvsInfo (view hoodleModeState uhdl)
	  cvs = view drawArea cvsInfo
	  msfc = view mDrawSurface cvsInfo 
      case epage of 
	Left page -> do  
	  liftIO (unSinglePageDraw drawf cache isCurrentCvs (cvs,msfc) (cpn,page)
		  <$> view viewInfo <*> pure mbbox <*> pure flag $ cvsInfo )
	  return ()
	Right tpage -> do 
	  liftIO (unSinglePageDraw drawfsel cache isCurrentCvs (cvs,msfc) (cpn,tpage)
		  <$> view viewInfo <*> pure mbbox <*> pure flag $ cvsInfo )
	  return ()
    fcont :: RenderCache -> UnitHoodle -> CanvasInfo ContinuousPage -> MainCoroutine () 
    fcont cache uhdl cvsInfo = do 
      let hdlmodst = view hoodleModeState uhdl 
	  isCurrentCvs = cid == getCurrentCanvasId uhdl
      case hdlmodst of 
	ViewAppendState hdl -> do  
	  hdl' <- liftIO (unContPageDraw drawcont cache isCurrentCvs cvsInfo mbbox hdl flag)
	  -- modify (set unitHoodles (putTheUnit (set hoodleModeState (ViewAppendState hdl') uhdl)))
          pureUpdateUhdl (const ((hoodleModeState .~ ViewAppendState hdl') uhdl))
	SelectState thdl -> do 
	  thdl' <- liftIO (unContPageDraw drawcontsel cache isCurrentCvs cvsInfo mbbox thdl flag)
	  -- modify (set unitHoodles (putTheUnit (set hoodleModeState (SelectState thdl') uhdl)))
          pureUpdateUhdl (const ((hoodleModeState .~ SelectState thdl') uhdl))
          
-- |         

invalidateOther :: MainCoroutine () 
invalidateOther = do 
  xst <- get
  let uhdl = view (unitHoodles.currentUnit) xst
      currCvsId = getCurrentCanvasId uhdl
      cinfoMap  = view cvsInfoMap uhdl
      keys = M.keys cinfoMap 
  mapM_ invalidate (filter (/=currCvsId) keys)
  
-- | invalidate clear 
invalidate :: CanvasId -> MainCoroutine () 
invalidate = invalidateInBBox Nothing Clear  

-- | 
invalidateInBBox :: Maybe BBox -- ^ desktop coord
                    -> DrawFlag 
                    -> CanvasId -> MainCoroutine ()
invalidateInBBox mbbox flag cid = do 
  xst <- get 
  let uhdl = view (unitHoodles.currentUnit) xst
  geometry <- liftIO $ getCanvasGeometryCvsId cid uhdl
  invalidateGeneral cid mbbox flag 
    (drawSinglePage geometry) (drawSinglePageSel geometry) (drawContHoodle geometry) (drawContHoodleSel geometry)

-- | 
invalidateAllInBBox :: Maybe BBox -- ^ desktop coordinate 
                       -> DrawFlag
                       -> MainCoroutine ()
invalidateAllInBBox mbbox flag = applyActionToAllCVS (invalidateInBBox mbbox flag)

-- | 
invalidateAll :: MainCoroutine () 
invalidateAll = invalidateAllInBBox Nothing Clear -- >> liftIO (putStrLn "The SLOW invalidateAll Called")
 
-- | Invalidate Current canvas
invalidateCurrent :: MainCoroutine () 
invalidateCurrent = invalidate . getCurrentCanvasId . view (unitHoodles.currentUnit) =<< get
       
-- | Drawing temporary gadgets
invalidateTemp :: CanvasId -> Cairo.Surface -> Cairo.Render () -> MainCoroutine ()
invalidateTemp cid tempsurface rndr = do 
    xst <- get 
    let uhdl = view (unitHoodles.currentUnit) xst
    forBoth' unboxBiAct (fsingle uhdl) . getCanvasInfo cid $ uhdl
  where 
    fsingle :: UnitHoodle -> CanvasInfo a -> MainCoroutine ()   
    fsingle uhdl cvsInfo = do 
      let canvas = view drawArea cvsInfo
          pnum = PageNum . view currentPageNum $ cvsInfo 
      geometry <- liftIO $ getCanvasGeometryCvsId cid uhdl
      win <- liftIO $ widgetGetDrawWindow canvas
      let xformfunc = cairoXform4PageCoordinate (mkXform4Page geometry pnum)
      liftIO $ renderWithDrawable win $ do   
                 Cairo.setSourceSurface tempsurface 0 0 
                 Cairo.setOperator Cairo.OperatorSource 
                 Cairo.paint 
                 xformfunc 
                 rndr 

-- | Drawing temporary gadgets with coordinate based on base page
invalidateTempBasePage :: CanvasId        -- ^ current canvas id
                       -> Cairo.Surface   -- ^ temporary cairo surface
                       -> PageNum         -- ^ current page number
                       -> Cairo.Render () -- ^ temporary rendering function
                       -> MainCoroutine ()
invalidateTempBasePage cid tempsurface pnum rndr = do 
    xst <- get 
    let uhdl = view (unitHoodles.currentUnit) xst
    forBoth' unboxBiAct (fsingle uhdl) . getCanvasInfo cid $ uhdl
  where 
    fsingle :: UnitHoodle -> CanvasInfo a -> MainCoroutine ()
    fsingle uhdl cvsInfo = do 
      let canvas = view drawArea cvsInfo
      geometry <- liftIO $ getCanvasGeometryCvsId cid uhdl
      win <- liftIO $ widgetGetDrawWindow canvas
      let xformfunc = cairoXform4PageCoordinate (mkXform4Page geometry pnum)
      liftIO $ renderWithDrawable win $ do   
                 Cairo.setSourceSurface tempsurface 0 0 
                 Cairo.setOperator Cairo.OperatorSource 
                 Cairo.paint 
                 xformfunc 
                 rndr 

  
-- | 
waitSomeEvent :: (UserEvent -> Bool) -> MainCoroutine UserEvent 
waitSomeEvent p = do 
    r <- nextevent
    case r of 
      UpdateCanvas cid -> -- this is temporary
        invalidateInBBox Nothing Efficient cid >> waitSomeEvent p  
      _ -> if  p r then return r else waitSomeEvent p  


callRenderer :: Renderer RenderEvent -> MainCoroutine ()
callRenderer action = do
    tvar <- (^. pdfRenderQueue) <$> get  
    doIOaction $ \evhandler -> do
      let handler = postGUIAsync . evhandler . SysEv . RenderCacheUpdate
      UsrEv . RenderEv <$> runReaderT action (handler,tvar)


callRenderer_ :: Renderer a -> MainCoroutine ()
callRenderer_ action = do
    callRenderer $ action >> return GotNone
    waitSomeEvent (\case RenderEv GotNone -> True ; _ -> False )
    return ()
