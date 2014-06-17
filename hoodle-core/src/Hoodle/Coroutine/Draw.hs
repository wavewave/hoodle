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
import           Control.Concurrent
-- import           Control.Concurrent.STM
import           Control.Lens (view,set,(^.),(%~))
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
import           Graphics.Hoodle.Render.Type.Renderer
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


-- | 
nextevent :: MainCoroutine UserEvent 
nextevent = do Arg DoEvent ev <- request (Res DoEvent ())
               case ev of
                 SysEv sev -> sysevent sev >> nextevent 
                 UsrEv uev -> return uev 

sysevent :: SystemEvent -> MainCoroutine () 
sysevent ClockUpdateEvent = do 
  utctime <- liftIO $ getCurrentTime 
  zone <- liftIO $ getCurrentTimeZone  
  let ltime = utcToLocalTime zone utctime 
      ltimeofday = localTimeOfDay ltime 
      (h,m,s) :: (Int,Int,Int) = 
        (,,) <$> (\x->todHour x `mod` 12) <*> todMin <*> (floor . todSec) 
        $ ltimeofday
  -- liftIO $ print (h,m,s)
  xst <- get 
  let cinfo = view currentCanvasInfo xst
      cwgts = view (unboxLens canvasWidgets) cinfo   
      nwgts = set (clockWidgetConfig.clockWidgetTime) (h,m,s) cwgts
      ncinfo = set (unboxLens canvasWidgets) nwgts cinfo
  put . set currentCanvasInfo ncinfo $ xst 
              
  when (view (widgetConfig.doesUseClockWidget) cwgts) $ do 
    let cid = getCurrentCanvasId xst
    modify (tempQueue %~ enqueue (Right (UsrEv (UpdateCanvasEfficient cid))))
    -- invalidateInBBox Nothing Efficient cid   
sysevent (RenderCacheUpdate (uuid, ssfc)) = do
  liftIO $ putStrLn "sysevent: RenderCacheUpdate event"
  modify (renderCache %~ HM.insert uuid ssfc)
  b <- ( ^. doesNotInvalidate ) <$> get
  when (not b) $ invalidateAll
  -- invalidateInBBox Nothing Efficient cid   
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
    unboxBiAct (fsingle xst) (fcont xst) . getCanvasInfo cid $ xst
  where 
    fsingle :: HoodleState -> CanvasInfo SinglePage -> MainCoroutine () 
    fsingle xstate cvsInfo = do 
      let cpn = PageNum . view currentPageNum $ cvsInfo 
	  isCurrentCvs = cid == getCurrentCanvasId xstate
	  epage = getCurrentPageEitherFromHoodleModeState cvsInfo (view hoodleModeState xstate)
	  cvs = view drawArea cvsInfo
	  msfc = view mDrawSurface cvsInfo 
	  cache = view renderCache xstate
      case epage of 
	Left page -> do  
	  liftIO (unSinglePageDraw drawf cache isCurrentCvs (cvs,msfc) (cpn,page)
		  <$> view viewInfo <*> pure mbbox <*> pure flag $ cvsInfo )
	  return ()
	Right tpage -> do 
	  liftIO (unSinglePageDraw drawfsel cache isCurrentCvs (cvs,msfc) (cpn,tpage)
		  <$> view viewInfo <*> pure mbbox <*> pure flag $ cvsInfo )
	  return ()
    fcont :: HoodleState -> CanvasInfo ContinuousPage -> MainCoroutine () 
    fcont xstate cvsInfo = do 
      let hdlmodst = view hoodleModeState xstate 
	  isCurrentCvs = cid == getCurrentCanvasId xstate
	  cache = view renderCache xstate
      case hdlmodst of 
	ViewAppendState hdl -> do  
	  hdl' <- liftIO (unContPageDraw drawcont cache isCurrentCvs cvsInfo mbbox hdl flag)
	  put (set hoodleModeState (ViewAppendState hdl') xstate)
	SelectState thdl -> do 
	  thdl' <- liftIO (unContPageDraw drawcontsel cache isCurrentCvs cvsInfo mbbox thdl flag)
	  put (set hoodleModeState (SelectState thdl') xstate) 
          
-- |         

invalidateOther :: MainCoroutine () 
invalidateOther = do 
  xstate <- get
  let currCvsId = getCurrentCanvasId xstate
      cinfoMap  = getCanvasInfoMap xstate
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
  geometry <- liftIO $ getCanvasGeometryCvsId cid xst 
  invalidateGeneral cid mbbox flag 
    (drawSinglePage geometry) (drawSinglePageSel geometry) (drawContHoodle geometry) (drawContHoodleSel geometry)

-- | 
invalidateAllInBBox :: Maybe BBox -- ^ desktop coordinate 
                       -> DrawFlag
                       -> MainCoroutine ()
invalidateAllInBBox mbbox flag = applyActionToAllCVS (invalidateInBBox mbbox flag)

-- | 
invalidateAll :: MainCoroutine () 
invalidateAll = invalidateAllInBBox Nothing Clear >> liftIO (putStrLn "The SLOW invalidateAll Called")
 
-- | Invalidate Current canvas
invalidateCurrent :: MainCoroutine () 
invalidateCurrent = invalidate . getCurrentCanvasId =<< get
       
-- | Drawing temporary gadgets
invalidateTemp :: CanvasId -> Cairo.Surface -> Cairo.Render () -> MainCoroutine ()
invalidateTemp cid tempsurface rndr = do 
    xst <- get 
    forBoth' unboxBiAct (fsingle xst) . getCanvasInfo cid $ xst 
  where 
    fsingle :: HoodleState -> CanvasInfo a -> MainCoroutine ()   
    fsingle xstate cvsInfo = do 
      let canvas = view drawArea cvsInfo
          pnum = PageNum . view currentPageNum $ cvsInfo 
      geometry <- liftIO $ getCanvasGeometryCvsId cid xstate
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
    forBoth' unboxBiAct (fsingle xst) . getCanvasInfo cid $ xst 
  where 
    fsingle :: HoodleState -> CanvasInfo a -> MainCoroutine ()
    fsingle xstate cvsInfo = do 
      let canvas = view drawArea cvsInfo
      geometry <- liftIO $ getCanvasGeometryCvsId cid xstate
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
    liftIO $ putStrLn ("waitSomeEvent : " ++ show r)
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
