{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hoodle.Coroutine.Draw where

import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Lens (Lens', set, view, (%~), (.~), (^.))
import Control.Monad (unless, void, when)
import Control.Monad.State (get, gets, liftIO, modify)
import Control.Monad.Trans.Crtn (request)
import Control.Monad.Trans.Crtn.Object (Arg (..), Res (..))
import Control.Monad.Trans.Crtn.Queue (enqueue)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.HashMap.Strict as HM
import Data.Hoodle.BBox (BBox (..))
import qualified Data.IntMap as M
import Data.Time.Clock
  ( getCurrentTime,
  )
import Data.Time.LocalTime
  ( getCurrentTimeZone,
    localTimeOfDay,
    todHour,
    todMin,
    todSec,
    utcToLocalTime,
  )
import Graphics.Hoodle.Render.Type
  ( RenderCache,
    Renderer,
    RendererEvent (FinishCommandFor, SurfaceUpdate),
    RendererState (..),
  )
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk as Gtk
import Hoodle.Accessor
  ( applyActionToAllCVS,
    getCanvasGeometryCvsId,
    getCurrentPageEitherFromHoodleModeState,
    pureUpdateUhdl,
    renderCache,
  )
import Hoodle.Type.Alias (EditMode, SelectMode)
import Hoodle.Type.Canvas
  ( CanvasId,
    CanvasInfo (..),
    canvasId,
    canvasWidgets,
    currentPageNum,
    drawArea,
    forBoth',
    mDrawSurface,
    unboxBiAct,
    unboxLens,
    viewInfo,
  )
import Hoodle.Type.Coroutine
  ( MainCoroutine,
    MainOp (DoEvent),
    doIOaction,
  )
import Hoodle.Type.Enum (DrawFlag (Clear, Efficient))
import Hoodle.Type.Event
  ( AllEvent (SysEv, UsrEv),
    RenderEvent (..),
    SystemEvent (ClockUpdateEvent, RenderCacheUpdate),
    UIEvent (UIGetFlag),
    UserEvent
      ( ActionOrdered,
        RenderEv,
        UIEv,
        UpdateCanvas,
        UpdateCanvasEfficient
      ),
  )
import Hoodle.Type.HoodleState
  ( HoodleModeState (SelectState, ViewAppendState),
    HoodleState,
    UnitHoodle,
    currentCanvasInfo,
    currentUnit,
    cvsInfoMap,
    doesNotInvalidate,
    genRenderQueue,
    getCanvasInfo,
    getCurrentCanvasId,
    gtkUIManager,
    hoodleModeState,
    pdfRenderQueue,
    renderCacheVar,
    tempQueue,
    unitHoodles,
  )
import Hoodle.Type.PageArrangement
  ( PageNum (..),
    ViewMode (ContinuousPage, SinglePage),
  )
import Hoodle.Type.Widget
  ( clockWidgetConfig,
    clockWidgetTime,
    doesUseClockWidget,
    widgetConfig,
  )
import Hoodle.Util (msgShout)
import Hoodle.View.Draw
  ( ContPageDraw (..),
    DrawingFunction,
    SinglePageDraw (..),
    cairoXform4PageCoordinate,
    drawContHoodle,
    drawContHoodleSel,
    drawSinglePage,
    drawSinglePageSel,
    mkXform4Page,
  )

-- | all event
nextevent :: MainCoroutine UserEvent
nextevent = do
  Arg DoEvent ev <- request (Res DoEvent ())
  case ev of
    SysEv sev -> sysevent sev >> nextevent
    UsrEv uev -> return uev

-- | system event
sysevent :: SystemEvent -> MainCoroutine ()
sysevent ClockUpdateEvent = do
  utctime <- liftIO getCurrentTime
  zone <- liftIO getCurrentTimeZone
  let ltime = utcToLocalTime zone utctime
      ltimeofday = localTimeOfDay ltime
      (h, m, s) :: (Int, Int, Int) =
        (,,) <$> (\x -> todHour x `mod` 12) <*> todMin <*> (floor . todSec) $
          ltimeofday
  xst <- get
  let uhdl = view (unitHoodles . currentUnit) xst
      cinfo = view currentCanvasInfo uhdl
      cwgts = view (unboxLens canvasWidgets) cinfo
      nwgts = set (clockWidgetConfig . clockWidgetTime) (h, m, s) cwgts
      ncinfo = set (unboxLens canvasWidgets) nwgts cinfo
  pureUpdateUhdl (const ((currentCanvasInfo .~ ncinfo) uhdl))
  when (view (widgetConfig . doesUseClockWidget) cwgts) $ do
    let cid = getCurrentCanvasId uhdl
    modify (tempQueue %~ enqueue (Right (UsrEv (UpdateCanvasEfficient cid))))
sysevent (RenderCacheUpdate (uuid, ssfc)) = do
  cachevar <- gets (view renderCacheVar)
  liftIO $ atomically $ modifyTVar' cachevar (HM.insert uuid ssfc)
  b <- gets (^. doesNotInvalidate)
  unless b invalidateAll
sysevent ev = msgShout (show ev)

-- | update flag in HoodleState when corresponding toggle UI changed
updateFlagFromToggleUI ::
  -- | UI toggle button id
  String ->
  -- | lens for flag
  Lens' HoodleState Bool ->
  MainCoroutine Bool
updateFlagFromToggleUI toggleid lensforflag = do
  xstate <- get
  let ui = view gtkUIManager xstate
  doIOaction $ \_ -> do
    agr <-
      Gtk.uiManagerGetActionGroups ui >>= \case
        [] -> error "No action group? "
        y : _ -> return y
    togglea <-
      Gtk.actionGroupGetAction agr toggleid
        >>= maybe
          (error "updateFlagFromToggleUI")
          (return . Gtk.castToToggleAction)
    b <- Gtk.toggleActionGetActive togglea
    return (UsrEv (UIEv (UIGetFlag b)))
  UIEv (UIGetFlag b) <-
    waitSomeEvent (\case UIEv (UIGetFlag _) -> True; _ -> False)
  modify (set lensforflag b) >> return b

-- |
data DrawingFunctionSet = DrawingFunctionSet
  { singleEditDraw :: DrawingFunction 'SinglePage EditMode,
    singleSelectDraw :: DrawingFunction 'SinglePage SelectMode,
    contEditDraw :: DrawingFunction 'ContinuousPage EditMode,
    contSelectDraw :: DrawingFunction 'ContinuousPage SelectMode
  }

-- |
invalidateGeneral ::
  CanvasId ->
  Maybe BBox ->
  DrawFlag ->
  DrawingFunction 'SinglePage EditMode ->
  DrawingFunction 'SinglePage SelectMode ->
  DrawingFunction 'ContinuousPage EditMode ->
  DrawingFunction 'ContinuousPage SelectMode ->
  MainCoroutine ()
invalidateGeneral cid mbbox flag drawf drawfsel drawcont drawcontsel = do
  xst <- get
  let uhdl = view (unitHoodles . currentUnit) xst
  cache <- renderCache
  unboxBiAct (fsingle cache uhdl) (fcont cache uhdl) . getCanvasInfo cid $ uhdl
  where
    fsingle :: RenderCache -> UnitHoodle -> CanvasInfo 'SinglePage -> MainCoroutine ()
    fsingle cache uhdl cvsInfo = do
      let cpn = PageNum . view currentPageNum $ cvsInfo
          isCurrentCvs = cid == getCurrentCanvasId uhdl
          epage = getCurrentPageEitherFromHoodleModeState cvsInfo (view hoodleModeState uhdl)
          cvsid = view canvasId cvsInfo
          cvs = view drawArea cvsInfo
          msfc = view mDrawSurface cvsInfo
      case epage of
        Left page ->
          void $
            liftIO
              ( unSinglePageDraw drawf cache cvsid isCurrentCvs (cvs, msfc) (cpn, page)
                  <$> view viewInfo <*> pure mbbox <*> pure flag
                  $ cvsInfo
              )
        Right tpage ->
          void $
            liftIO
              ( unSinglePageDraw drawfsel cache cvsid isCurrentCvs (cvs, msfc) (cpn, tpage)
                  <$> view viewInfo <*> pure mbbox <*> pure flag
                  $ cvsInfo
              )
    fcont :: RenderCache -> UnitHoodle -> CanvasInfo 'ContinuousPage -> MainCoroutine ()
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
  let uhdl = view (unitHoodles . currentUnit) xst
      currCvsId = getCurrentCanvasId uhdl
      cinfoMap = view cvsInfoMap uhdl
      keys = M.keys cinfoMap
  mapM_ invalidate (filter (/= currCvsId) keys)

-- | invalidate clear
invalidate :: CanvasId -> MainCoroutine ()
invalidate = invalidateInBBox Nothing Clear

-- |
invalidateInBBox ::
  -- | desktop coord
  Maybe BBox ->
  DrawFlag ->
  CanvasId ->
  MainCoroutine ()
invalidateInBBox mbbox flag cid = do
  xst <- get
  let uhdl = view (unitHoodles . currentUnit) xst
  geometry <- liftIO $ getCanvasGeometryCvsId cid uhdl
  invalidateGeneral
    cid
    mbbox
    flag
    (drawSinglePage geometry)
    (drawSinglePageSel geometry)
    (drawContHoodle geometry)
    (drawContHoodleSel geometry)

-- |
invalidateAllInBBox ::
  -- | desktop coordinate
  Maybe BBox ->
  DrawFlag ->
  MainCoroutine ()
invalidateAllInBBox mbbox flag = applyActionToAllCVS (invalidateInBBox mbbox flag)

-- |
invalidateAll :: MainCoroutine ()
invalidateAll = invalidateAllInBBox Nothing Clear

-- | Invalidate Current canvas
invalidateCurrent :: MainCoroutine ()
invalidateCurrent = invalidate . getCurrentCanvasId . view (unitHoodles . currentUnit) =<< get

-- | Drawing temporary gadgets
invalidateTemp :: CanvasId -> Cairo.Surface -> Cairo.Render () -> MainCoroutine ()
invalidateTemp cid tempsurface rndr = do
  xst <- get
  let uhdl = view (unitHoodles . currentUnit) xst
  forBoth' unboxBiAct (fsingle uhdl) . getCanvasInfo cid $ uhdl
  where
    fsingle :: UnitHoodle -> CanvasInfo a -> MainCoroutine ()
    fsingle uhdl cvsInfo = do
      let canvas = view drawArea cvsInfo
          pnum = PageNum . view currentPageNum $ cvsInfo
      geometry <- liftIO $ getCanvasGeometryCvsId cid uhdl
      Just win <- liftIO $ Gtk.widgetGetWindow canvas
      let xformfunc = cairoXform4PageCoordinate (mkXform4Page geometry pnum)
      liftIO $
        Gtk.renderWithDrawWindow win $ do
          Cairo.setSourceSurface tempsurface 0 0
          Cairo.setOperator Cairo.OperatorSource
          Cairo.paint
          xformfunc
          rndr

-- | Drawing temporary gadgets with coordinate based on base page
invalidateTempBasePage ::
  -- | current canvas id
  CanvasId ->
  -- | temporary cairo surface
  Cairo.Surface ->
  -- | current page number
  PageNum ->
  -- | temporary rendering function
  Cairo.Render () ->
  MainCoroutine ()
invalidateTempBasePage cid tempsurface pnum rndr = do
  xst <- get
  let uhdl = view (unitHoodles . currentUnit) xst
  forBoth' unboxBiAct (fsingle uhdl) . getCanvasInfo cid $ uhdl
  where
    fsingle :: UnitHoodle -> CanvasInfo a -> MainCoroutine ()
    fsingle uhdl cvsInfo = do
      let canvas = view drawArea cvsInfo
      geometry <- liftIO $ getCanvasGeometryCvsId cid uhdl
      Just win <- liftIO $ Gtk.widgetGetWindow canvas
      let xformfunc = cairoXform4PageCoordinate (mkXform4Page geometry pnum)
      liftIO $
        Gtk.renderWithDrawWindow win $ do
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
    UpdateCanvas cid ->
      -- this is temporary
      invalidateInBBox Nothing Efficient cid >> waitSomeEvent p
    _ -> if p r then return r else waitSomeEvent p

-- |
doIOaction_ :: IO a -> MainCoroutine ()
doIOaction_ action = do
  doIOaction $ \_ -> action >> return (UsrEv ActionOrdered)
  void $ waitSomeEvent (\case ActionOrdered -> True; _ -> False)

defaultHandler :: (AllEvent -> IO ()) -> RendererEvent -> IO ()
defaultHandler evhandler (SurfaceUpdate s) =
  Gtk.postGUIAsync . evhandler . SysEv . RenderCacheUpdate $ s
defaultHandler evhandler (FinishCommandFor sfcid) =
  Gtk.postGUIAsync . evhandler . UsrEv . RenderEv . FinishCommand $ sfcid

-- | order rendering routine
callRenderer :: Renderer RenderEvent -> MainCoroutine ()
callRenderer action = do
  (tvarpdf, tvargen, tvarcache) <-
    gets ((,,) <$> (^. pdfRenderQueue) <*> (^. genRenderQueue) <*> (^. renderCacheVar))
  doIOaction $ \evhandler ->
    UsrEv . RenderEv <$> runReaderT action (RendererState (defaultHandler evhandler) tvarpdf tvargen tvarcache)

callRenderer_ :: Renderer a -> MainCoroutine ()
callRenderer_ action = do
  callRenderer $ action >> return GotNone
  void $ waitSomeEvent (\case RenderEv GotNone -> True; _ -> False)
