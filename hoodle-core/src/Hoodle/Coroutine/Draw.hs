{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Draw 
-- Copyright   : (c) 2011-2015 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Draw where

-- from other packages
import           Control.Applicative
import           Control.Concurrent.STM (atomically, modifyTVar')
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Reader (runReaderT)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as M
import           Data.Time.Clock
import           Data.Time.LocalTime
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk as Gtk
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
import           Hoodle.Util
import           Hoodle.View.Draw
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
  pureUpdateUhdl (const ((currentCanvasInfo .~ ncinfo) uhdl))
  when (view (widgetConfig.doesUseClockWidget) cwgts) $ do 
    let cid = getCurrentCanvasId uhdl
    modify (tempQueue %~ enqueue (Right (UsrEv (UpdateCanvasEfficient cid))))
sysevent (RenderCacheUpdate (uuid, ssfc)) = do
  cachevar <- view renderCacheVar <$> get
  liftIO $ atomically $ modifyTVar' cachevar (HM.insert uuid ssfc) 
  b <- ( ^. doesNotInvalidate ) <$> get
  when (not b) $ invalidateAll
sysevent ev = msgShout (show ev)

-- | update flag in HoodleState when corresponding toggle UI changed 
updateFlagFromToggleUI :: String  -- ^ UI toggle button id
                       -> Simple Lens HoodleState Bool -- ^ lens for flag 
                       -> MainCoroutine Bool
updateFlagFromToggleUI toggleid lensforflag = do 
  xstate <- get 
  let ui = view gtkUIManager xstate 
  doIOaction $ \_ -> do
    agr <- Gtk.uiManagerGetActionGroups ui >>= \x ->
             case x of 
               [] -> error "No action group? "
               y:_ -> return y 
    togglea <- Gtk.actionGroupGetAction agr toggleid 
                 >>= maybe (error "updateFlagFromToggleUI") 
                           (return . Gtk.castToToggleAction)
    b <- Gtk.toggleActionGetActive togglea
    return (UsrEv (UIEv (UIGetFlag b)))
  UIEv (UIGetFlag b) <- 
    waitSomeEvent (\case UIEv (UIGetFlag _) -> True ; _ -> False) 
  modify (set lensforflag b) >> return b 
  
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
    cache <- renderCache
    unboxBiAct (fsingle cache uhdl) (fcont cache uhdl) . getCanvasInfo cid $ uhdl
  where 
    fsingle :: RenderCache -> UnitHoodle -> CanvasInfo SinglePage -> MainCoroutine () 
    fsingle cache uhdl cvsInfo = do 
      let cpn = PageNum . view currentPageNum $ cvsInfo 
	  isCurrentCvs = cid == getCurrentCanvasId uhdl
	  epage = getCurrentPageEitherFromHoodleModeState cvsInfo (view hoodleModeState uhdl)
          cvsid = view canvasId cvsInfo
	  cvs = view drawArea cvsInfo
	  msfc = view mDrawSurface cvsInfo 
      case epage of 
	Left page -> do  
	  liftIO (unSinglePageDraw drawf cache cvsid isCurrentCvs (cvs,msfc) (cpn,page)
		  <$> view viewInfo <*> pure mbbox <*> pure flag $ cvsInfo )
	  return ()
	Right tpage -> do 
	  liftIO (unSinglePageDraw drawfsel cache cvsid isCurrentCvs (cvs,msfc) (cpn,tpage)
		  <$> view viewInfo <*> pure mbbox <*> pure flag $ cvsInfo )
	  return ()
    fcont :: RenderCache -> UnitHoodle -> CanvasInfo ContinuousPage -> MainCoroutine () 
    fcont cache uhdl cvsInfo = do 
      let hdlmodst = view hoodleModeState uhdl 
	  isCurrentCvs = cid == getCurrentCanvasId uhdl
      case hdlmodst of 
	ViewAppendState hdl -> do  
	  hdl' <- liftIO (unContPageDraw drawcont cache isCurrentCvs cvsInfo mbbox hdl flag)
          pureUpdateUhdl (const ((hoodleModeState .~ ViewAppendState hdl') uhdl))
	SelectState thdl -> do 
	  thdl' <- liftIO (unContPageDraw drawcontsel cache isCurrentCvs cvsInfo mbbox thdl flag)
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
invalidateAll = invalidateAllInBBox Nothing Clear
 
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
#ifdef GTK3          
      Just win <- liftIO $ Gtk.widgetGetWindow canvas
#else
      win <- liftIO $ Gtk.widgetGetDrawWindow canvas
#endif 
      let xformfunc = cairoXform4PageCoordinate (mkXform4Page geometry pnum)
#ifdef GTK3              
      liftIO $ Gtk.renderWithDrawWindow win $ do 
#else 
      liftIO $ Gtk.renderWithDrawable win $ do   
#endif
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
#ifdef GTK3          
      Just win <- liftIO $ Gtk.widgetGetWindow canvas
#else
      win <- liftIO $ Gtk.widgetGetDrawWindow canvas
#endif
      let xformfunc = cairoXform4PageCoordinate (mkXform4Page geometry pnum)
#ifdef GTK3              
      liftIO $ Gtk.renderWithDrawWindow win $ do   
#else 
      liftIO $ Gtk.renderWithDrawable win $ do   
#endif
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

-- | 
doIOaction_ :: IO a -> MainCoroutine ()
doIOaction_ action = do doIOaction $ \_ -> action >> return (UsrEv ActionOrdered)
                        waitSomeEvent (\case ActionOrdered -> True; _ -> False );
                        return ()

defaultHandler :: (AllEvent -> IO ()) -> RendererEvent -> IO ()
defaultHandler evhandler (SurfaceUpdate s) = 
    Gtk.postGUIAsync . evhandler . SysEv . RenderCacheUpdate $ s
defaultHandler evhandler (FinishCommandFor sfcid) =  
    Gtk.postGUIAsync . evhandler . UsrEv . RenderEv . FinishCommand $ sfcid


-- | order rendering routine
callRenderer :: Renderer RenderEvent -> MainCoroutine ()
callRenderer action = do
    (tvarpdf,tvargen,tvarcache) <- ((,,)<$>(^. pdfRenderQueue)<*>(^. genRenderQueue)<*>(^. renderCacheVar))<$>get  
    doIOaction $ \evhandler -> do
      -- let handler (SurfaceUpdate s) = 
      --       Gtk.postGUIAsync . evhandler . SysEv . RenderCacheUpdate $ s
      --     handler (FinishCommandFor sfcid) =
      --       Gtk.postGUIAsync . evhandler . UsrEv . RenderEv . FinishCommand $ sfcid
      UsrEv . RenderEv <$> runReaderT action (RendererState (defaultHandler evhandler) tvarpdf tvargen tvarcache)

callRenderer_ :: Renderer a -> MainCoroutine ()
callRenderer_ action = do
    callRenderer $ action >> return GotNone
    waitSomeEvent (\case RenderEv GotNone -> True ; _ -> False )
    return ()
