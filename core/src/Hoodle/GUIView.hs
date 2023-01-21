{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -w #-}

module Hoodle.GUIView (startGUIView) where

import Control.Concurrent
  ( forkIO,
    forkOn,
    newMVar,
    newEmptyMVar,
    putMVar,
    takeMVar,
    threadDelay,
  )
import Control.Exception (SomeException (..), catch)
import Control.Lens (at, over, set, view, (.~), (?~), (^.), _2)
import Control.Monad (forever, return, void, when, (>>), (>>=))
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Crtn.Driver (driver)
import Control.Monad.Trans.Crtn.Logger.Simple (simplelogger)
import Control.Monad.Trans.Crtn.Object (Arg (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Foldable (forM_, traverse_)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.IntMap as M
import Data.Maybe (Maybe (..), maybe)
import Data.Time.Clock (diffUTCTime, getCurrentTime, secondsToNominalDiffTime)
import qualified Graphics.UI.Gtk as Gtk
import Hoodle.Accessor
  ( lensSetToggleUIForFlag,
    setToggleUIForFlag,
  )
import Hoodle.Config
  ( getMaxUndo,
    getPenConfig,
    getWidgetConfig,
    getXInputConfig,
    loadConfigFile,
  )
import Hoodle.Coroutine.Callback (eventHandler)
import Hoodle.Coroutine.Default (guiProcess)
import Hoodle.Device
  ( DeviceList,
    PenButton (..),
    dev_touch_str,
    initDevice,
  )
import Hoodle.GUI.Menu
  ( getMenuUI,
    int2Point,
  )
import Hoodle.ModelAction.Window (constructFrame, createTab, eventConnect, setTitleFromFileName)
import Hoodle.Type.Canvas
  ( CanvasId,
    CanvasInfoBox (CanvasSinglePage),
    canvasWidgets,
    currentPageNum,
    currentTool,
    defaultCvsInfoSinglePage,
    penColor,
    penType,
    penWidth,
    unboxLens,
    _canvasId,
  )
import Hoodle.Type.Coroutine
  ( EventVar,
    MainCoroutine,
    MainOp (DoEvent),
    doIOaction,
    world,
  )
import Hoodle.Type.Event
  ( AllEvent (..),
    MenuEvent (MenuQuit),
    NetworkEvent (..),
    RenderEvent (..),
    UserEvent (..),
  )
import Hoodle.Type.HoodleState
  ( FileStore (LocalDir),
    HoodleModeState (ViewAppendState),
    HoodleState,
    IsOneTimeSelectMode (YesBeforeSelect),
    backgroundStyle,
    callBack,
    currentCanvasInfo,
    currentUnit,
    cvsInfoMap,
    deviceList,
    doesEmbedImage,
    doesEmbedPDF,
    doesUsePopUpMenu,
    doesUseTouch,
    doesUseVariableCursor,
    doesUseXInput,
    emptyHoodleState,
    frameState,
    genRenderQueue,
    getCurrentCanvasId,
    getHoodle,
    gtkUIManager,
    hoodleFileControl,
    hoodleFileName,
    hoodleModeState,
    hoodleModeStateEither,
    hookSet,
    isOneTimeSelectMode,
    isSaved,
    newPageMode,
    pdfRenderQueue,
    penInfo,
    renderCacheVar,
    resetHoodleModeStateBuffers,
    rootContainer,
    rootNotebook,
    rootOfRootWindow,
    rootWindow,
    selectInfo,
    settings,
    statusBar,
    switchTabSignal,
    uiComponentSignalHandler,
    undoTable,
    unitButton,
    unitHoodles,
    unitUUID,
    updateFromCanvasInfoAsCurrentCanvas,
  )
import Hoodle.Type.Undo (emptyUndo)
import Hoodle.Type.Widget
  ( doesUseLayerWidget,
    doesUsePanZoomWidget,
    widgetConfig,
  )
import Hoodle.Type.Window (WindowConfig (Node))
import Hoodle.Util (msgShout, (#))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO
  ( FilePath,
    IO,
    IOMode (WriteMode),
    hClose,
    hPutStrLn,
    openFile,
  )
import System.Directory (canonicalizePath)
import System.FilePath (takeDirectory)
import System.FSNotify qualified as FS

-- |
startGUIView :: Maybe FilePath -> IO ()
startGUIView mfname = do
  _ <- Gtk.initGUI
  window <- Gtk.windowNew
  Gtk.windowSetDefaultSize window 800 400
  cfg <- loadConfigFile
  devlst <- initDevice cfg
  xinputbool <- getXInputConfig cfg
  varcsr <- getPenConfig cfg
  (usepz, uselyr) <- getWidgetConfig cfg
  (tref, st0, ui, vbox) <-
    initCoroutineForViewer devlst window (xinputbool, usepz, uselyr, varcsr)
  setTitleFromFileName st0
  void $ lensSetToggleUIForFlag "UXINPUTA" (settings . doesUseXInput) st0
  void $ lensSetToggleUIForFlag "HANDA" (settings . doesUseTouch) st0
  void $ lensSetToggleUIForFlag "POPMENUA" (settings . doesUsePopUpMenu) st0
  void $ lensSetToggleUIForFlag "EBDIMGA" (settings . doesEmbedImage) st0
  void $ lensSetToggleUIForFlag "EBDPDFA" (settings . doesEmbedPDF) st0
  _ <- setToggleUIForFlag "TOGGLENETSRCA" False st0
  --
  outerLayoutForViewer ui vbox st0
  _ <- window `Gtk.on` Gtk.deleteEvent $ do
    liftIO $ eventHandler tref (UsrEv (Menu MenuQuit))
    return True
  Gtk.widgetShowAll window
  eventHandler tref (UsrEv (Initialized mfname))
  forM_ mfname $ \fname -> do
    fname' <- canonicalizePath fname
    fileWatcher (eventHandler tref) fname'
  Gtk.mainGUI
  pure ()

-- |
initCoroutineForViewer ::
  DeviceList ->
  Gtk.Window ->
  -- | (xinputbool,usepz,uselyr,varcsr)
  (Bool, Bool, Bool, Bool) ->
  IO (EventVar, HoodleState, Gtk.UIManager, Gtk.VBox)
initCoroutineForViewer devlst window (xinputbool, usepz, uselyr, varcsr) = do
  evar <- newEmptyMVar
  putMVar evar Nothing
  let callback = eventHandler evar
  st0new <-
    set deviceList devlst
      . set rootOfRootWindow window
      . set callBack callback
      <$> emptyHoodleState
  (ui, uicompsighdlr) <- getMenuUI evar
  let st1 = set gtkUIManager ui st0new
      initcvs =
        set (canvasWidgets . widgetConfig . doesUsePanZoomWidget) usepz
          . set (canvasWidgets . widgetConfig . doesUseLayerWidget) uselyr
          $ defaultCvsInfoSinglePage {_canvasId = 1}
      initcvsbox = CanvasSinglePage initcvs
      st2 =
        st1
          # over
            (unitHoodles . currentUnit)
            ( set frameState (Node 1)
                . updateFromCanvasInfoAsCurrentCanvas initcvsbox
                . set cvsInfoMap M.empty
            )
      uhdl2 = view (unitHoodles . currentUnit) st2
  (uhdl3, rtwdw, _wconf) <- constructFrame st2 uhdl2 (view frameState uhdl2)
  (uhdl4, wconf') <- eventConnect st2 uhdl3 (view frameState uhdl3)
  notebook <- Gtk.notebookNew
  statusbar <- Gtk.statusbarNew
  let st4 = (unitHoodles . currentUnit .~ uhdl4) st2
      st5 =
        st4
          # over
            (unitHoodles . currentUnit)
            ( set undoTable (emptyUndo 1)
                . set frameState wconf'
                . set rootWindow rtwdw
                . set (hoodleFileControl . hoodleFileName) (LocalDir Nothing)
            )
            . set (settings . doesUseXInput) xinputbool
            . set (settings . doesUseVariableCursor) varcsr
            . set hookSet Nothing
            . set rootNotebook notebook
            . set uiComponentSignalHandler uicompsighdlr
            . set statusBar (Just statusbar)
  --
  vbox <- Gtk.vBoxNew False 0
  Gtk.containerAdd window vbox
  vboxcvs <- Gtk.vBoxNew False 0
  (_, uuid, btn) <- createTab callback notebook vboxcvs
  Gtk.containerAdd vboxcvs (view (unitHoodles . currentUnit . rootWindow) st5)
  --
  sigid <- notebook `Gtk.on` Gtk.switchPage $ \i -> callback (UsrEv (SwitchTab i))
  let st6 =
        ( (unitHoodles . currentUnit . unitUUID .~ uuid)
            . (unitHoodles . currentUnit . unitButton .~ btn)
            . (uiComponentSignalHandler . switchTabSignal ?~ sigid)
        )
          st5
      startingXstate = (unitHoodles . currentUnit . rootContainer .~ Gtk.castToBox vboxcvs) st6
      startworld = world startingXstate . ReaderT $ (\(Arg DoEvent ev) -> guiProcess ev)
  putMVar evar . Just $ driver simplelogger startworld
  return (evar, startingXstate, ui, vbox)

outerLayoutForViewer :: Gtk.UIManager -> Gtk.VBox -> HoodleState -> IO ()
outerLayoutForViewer ui vbox xst = do
  let notebook = view rootNotebook xst
      mstatusbar = view statusBar xst
  toolbar1 <-
    Gtk.uiManagerGetWidget ui "/ui/toolbar1"
      >>= maybe (error "GUI.hs:no toolbar1") return
  Gtk.boxPackStart vbox toolbar1 Gtk.PackNatural 0
  forM_ mstatusbar $ \statusbar -> Gtk.boxPackEnd vbox statusbar Gtk.PackNatural 0
  --
  Gtk.boxPackStart vbox notebook Gtk.PackGrow 0

fileWatcher :: (AllEvent -> IO ()) -> FilePath -> IO ()
fileWatcher evhandler fp = do
  let dir = takeDirectory fp
  forkIO $ do
    FS.withManager $ \mgr -> do
      t0 <- getCurrentTime
      ref <- newIORef t0
      let pred (FS.Modified fp' _ _) = fp == fp'
          pred _ = False
          action ev@(FS.Modified fp' t' _) = do
            t <- readIORef ref
            let delta = diffUTCTime t' t
            when (delta > secondsToNominalDiffTime 1) $ do
              print ev
              -- this is a hack. wait enough for file to be saved.
              threadDelay 1_000_000
              t'' <- getCurrentTime
              writeIORef ref t''
              Gtk.postGUIAsync $ evhandler $ UsrEv $ FileReloadOrdered
      FS.watchDir mgr dir pred action
      forever (threadDelay 1_000_000)
    pure ()
  pure ()
