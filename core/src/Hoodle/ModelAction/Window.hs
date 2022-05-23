{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hoodle.ModelAction.Window where

import Control.Lens (view)
import Control.Monad hiding (forM_)
import Control.Monad.Trans
import Data.Foldable (forM_, traverse_)
import Data.IORef (newIORef, readIORef)
import qualified Data.IntMap as M
import Data.Maybe (fromMaybe)
import Data.UUID (UUID)
import Data.UUID.V4
import qualified Graphics.UI.Gtk as Gtk
import Hoodle.Device
import Hoodle.Type.Canvas
import Hoodle.Type.Event
import Hoodle.Type.HoodleState
import Hoodle.Type.Window
import Hoodle.Util
import System.FilePath

-- | set frame title according to file name
setTitleFromFileName :: HoodleState -> IO ()
setTitleFromFileName xstate = do
  case view (unitHoodles . currentUnit . hoodleFileControl . hoodleFileName) xstate of
    LocalDir Nothing ->
      Gtk.set
        (view rootOfRootWindow xstate)
        [Gtk.windowTitle Gtk.:= ("untitled" :: String)]
    LocalDir (Just filename) ->
      Gtk.set
        (view rootOfRootWindow xstate)
        [Gtk.windowTitle Gtk.:= takeFileName filename]
    TempDir _filename ->
      Gtk.set
        (view rootOfRootWindow xstate)
        [Gtk.windowTitle Gtk.:= ("shared document" :: String)]

-- |
newCanvasId :: CanvasInfoMap -> CanvasId
newCanvasId cmap = let cids = M.keys cmap in maximum cids + 1

-- | initialize CanvasInfo with creating windows and connect events
initCanvasInfo :: HoodleState -> UnitHoodle -> CanvasId -> IO (CanvasInfo a)
initCanvasInfo xstate uhdl cid =
  minimalCanvasInfo cid >>= connectDefaultEventCanvasInfo xstate uhdl

-- | only creating windows
minimalCanvasInfo :: CanvasId -> IO (CanvasInfo a)
minimalCanvasInfo cid = do
  canvas <- Gtk.drawingAreaNew
  hadj <- Gtk.adjustmentNew 0 0 500 100 200 200
  vadj <- Gtk.adjustmentNew 0 0 500 100 200 200
  vbox <- Gtk.vBoxNew False 0
  hscr <- Gtk.hScrollbarNew hadj
  hbox <- Gtk.hBoxNew False 0
  vscr <- Gtk.vScrollbarNew vadj
  Gtk.boxPackStart hbox canvas Gtk.PackGrow 0
  Gtk.boxPackEnd hbox vscr Gtk.PackNatural 0
  Gtk.boxPackStart vbox hbox Gtk.PackGrow 0
  Gtk.boxPackEnd vbox hscr Gtk.PackNatural 0
  let scrwin = MyScrollWindow vbox hscr vscr
  return $ CanvasInfo cid canvas Nothing scrwin (error "no viewInfo" :: ViewInfo a) 0 hadj vadj Nothing Nothing defaultCanvasWidgets Nothing

-- | only connect events
connectDefaultEventCanvasInfo ::
  HoodleState -> UnitHoodle -> CanvasInfo a -> IO (CanvasInfo a)
connectDefaultEventCanvasInfo xstate _uhdl cinfo = do
  let callback = view callBack xstate
      ui = view gtkUIManager xstate
      dev = view deviceList xstate
      canvas = _drawArea cinfo
      cid = _canvasId cinfo
      scrwin = _scrolledWindow cinfo
      hadj = _horizAdjustment cinfo
      vadj = _vertAdjustment cinfo
  Gtk.widgetSetCanFocus canvas True
  Gtk.widgetGrabFocus canvas
  _confevent <- canvas `Gtk.on` Gtk.configureEvent $
    Gtk.tryEvent $ do
      (w, h) <- Gtk.eventSize
      liftIO $ callback (UsrEv (CanvasConfigure cid (fromIntegral w) (fromIntegral h)))
  _keyevent <- canvas `Gtk.on` Gtk.keyPressEvent $
    Gtk.tryEvent $ do
      m <- Gtk.eventModifier
      n <- Gtk.eventKeyName
      let keystr = show m ++ ":" ++ show n
      liftIO $ callback (UsrEv (CustomKeyEvent keystr))
  _bpevent <- canvas `Gtk.on` Gtk.buttonPressEvent $
    Gtk.tryEvent $ do
      liftIO $ Gtk.widgetGrabFocus canvas
      (mbtn, mp) <- getPointer dev
      forM_ mp $ \p -> do
        let pbtn = fromMaybe PenButton1 mbtn
        case pbtn of
          TouchButton -> liftIO (callback (UsrEv (TouchDown cid p)))
          _ -> liftIO (callback (UsrEv (PenDown cid pbtn p)))
  _brevent <- canvas `Gtk.on` Gtk.buttonReleaseEvent $
    Gtk.tryEvent $ do
      (mbtn, mp) <- getPointer dev
      forM_ mp $ \p -> do
        let pbtn = fromMaybe PenButton1 mbtn
        case pbtn of
          TouchButton -> (liftIO . callback . UsrEv) (TouchUp cid p)
          _ -> (liftIO . callback . UsrEv) (PenUp cid p)
  _drawev <- canvas `Gtk.on` Gtk.draw $ do
    liftIO $ Gtk.widgetGrabFocus canvas
    (liftIO . callback . UsrEv) (UpdateCanvas cid)
  _ <- canvas `Gtk.on` Gtk.motionNotifyEvent $
    Gtk.tryEvent $ do
      (mbtn, mp) <- getPointer dev
      forM_ mp $ \p -> do
        let pbtn = fromMaybe PenButton1 mbtn
        case pbtn of
          TouchButton -> (liftIO . callback . UsrEv) (TouchMove cid p)
          _ -> (liftIO . callback . UsrEv) (PenMove cid p)
  -- drag and drop setting
  Gtk.dragDestSet canvas [Gtk.DestDefaultMotion, Gtk.DestDefaultDrop] [Gtk.ActionCopy]
  Gtk.dragDestAddTextTargets canvas
  _ <- canvas `Gtk.on` Gtk.dragDataReceived $ \_dc pos _i _ts -> do
    s <- Gtk.selectionDataGetText
    (liftIO . callback . UsrEv) (GotLink s pos)
  Gtk.widgetAddEvents canvas [Gtk.PointerMotionMask, Gtk.Button1MotionMask, Gtk.KeyPressMask]
  agr <-
    liftIO
      ( Gtk.uiManagerGetActionGroups ui >>= \case
          [] -> error "No action group? "
          y : _ -> return y
      )
  _uxinputa <-
    liftIO
      ( Gtk.actionGroupGetAction agr ("UXINPUTA" :: String) >>= \(Just x) ->
          return (Gtk.castToToggleAction x)
      )
  hadjconnid <- Gtk.afterValueChanged hadj $ do
    v <- Gtk.adjustmentGetValue hadj
    (callback . UsrEv) (HScrollBarMoved cid v)
  vadjconnid <- Gtk.afterValueChanged vadj $ do
    v <- Gtk.adjustmentGetValue vadj
    (callback . UsrEv) (VScrollBarMoved cid v)
  let vscrbar = _scrollVScrollbar scrwin
  _bpevtvscrbar <- vscrbar `Gtk.on` Gtk.buttonPressEvent $ do
    v <- liftIO $ Gtk.adjustmentGetValue vadj
    liftIO ((callback . UsrEv) (VScrollBarStart cid v))
    return False
  _brevtvscrbar <- vscrbar `Gtk.on` Gtk.buttonReleaseEvent $ do
    v <- liftIO $ Gtk.adjustmentGetValue vadj
    liftIO ((callback . UsrEv) (VScrollBarEnd cid v))
    return False
  return $
    cinfo
      { _horizAdjConnId = Just hadjconnid,
        _vertAdjConnId = Just vadjconnid
      }

-- | recreate windows from old canvas info but no event connect
reinitCanvasInfoStage1 ::
  UnitHoodle -> CanvasInfo a -> IO (CanvasInfo a)
reinitCanvasInfoStage1 _uhdl oldcinfo = do
  let cid = view canvasId oldcinfo
  newcinfo <- minimalCanvasInfo cid
  return $
    newcinfo
      { _viewInfo = _viewInfo oldcinfo,
        _currentPageNum = _currentPageNum oldcinfo
      }

-- | event connect
reinitCanvasInfoStage2 ::
  HoodleState -> UnitHoodle -> CanvasInfo a -> IO (CanvasInfo a)
reinitCanvasInfoStage2 = connectDefaultEventCanvasInfo

-- | event connecting for all windows
eventConnect :: HoodleState -> UnitHoodle -> WindowConfig -> IO (UnitHoodle, WindowConfig)
eventConnect xst uhdl (Node cid) = do
  let cmap = view cvsInfoMap uhdl
      cinfobox = maybeError' "eventConnect" $ M.lookup cid cmap
  ncinfobox <- forBoth unboxBiXform (reinitCanvasInfoStage2 xst uhdl) cinfobox
  let uhdl' = updateFromCanvasInfoAsCurrentCanvas ncinfobox uhdl
  return (uhdl', Node cid)
eventConnect xst uhdl (HSplit wconf1 wconf2) = do
  (uhdl', wconf1') <- eventConnect xst uhdl wconf1
  (uhdl'', wconf2') <- eventConnect xst uhdl' wconf2
  return (uhdl'', HSplit wconf1' wconf2')
eventConnect xst uhdl (VSplit wconf1 wconf2) = do
  (uhdl', wconf1') <- eventConnect xst uhdl wconf1
  (uhdl'', wconf2') <- eventConnect xst uhdl' wconf2
  return (uhdl'', VSplit wconf1' wconf2')

-- | default construct frame
constructFrame ::
  HoodleState ->
  UnitHoodle ->
  WindowConfig ->
  IO (UnitHoodle, Gtk.Widget, WindowConfig)
constructFrame xst uhdl wcfg =
  let callback = view callBack xst
   in constructFrame' callback (CanvasSinglePage defaultCvsInfoSinglePage) uhdl wcfg

-- | construct frames with template
constructFrame' ::
  (AllEvent -> IO ()) ->
  CanvasInfoBox ->
  UnitHoodle ->
  WindowConfig ->
  IO (UnitHoodle, Gtk.Widget, WindowConfig)
constructFrame' _callback template ouhdl (Node cid) = do
  let ocmap = view cvsInfoMap ouhdl
  (cinfobox, _cmap, uhdl) <- case M.lookup cid ocmap of
    Just cinfobox' -> return (cinfobox', ocmap, ouhdl)
    Nothing -> do
      let cinfobox' = setCanvasId cid template
          cmap' = M.insert cid cinfobox' ocmap
          uhdl' = fromMaybe ouhdl (setCanvasInfoMap cmap' ouhdl)
      return (cinfobox', cmap', uhdl')
  ncinfobox <- forBoth unboxBiXform (reinitCanvasInfoStage1 uhdl) cinfobox
  let uhdl' = updateFromCanvasInfoAsCurrentCanvas ncinfobox uhdl
  forBoth' unboxBiAct (putStrLn <=< Gtk.widgetGetName . view drawArea) ncinfobox
  let scrwin = forBoth' unboxBiAct (Gtk.castToWidget . _scrollCanvas . view scrolledWindow) ncinfobox
  return (uhdl', scrwin, Node cid)
constructFrame' callback template uhdl (HSplit wconf1 wconf2) = do
  (uhdl', win1, wconf1') <- constructFrame' callback template uhdl wconf1
  (uhdl'', win2, wconf2') <- constructFrame' callback template uhdl' wconf2
  hpane' <- Gtk.hPanedNew
  _ <- hpane' `Gtk.on` Gtk.buttonPressEvent $ do
    liftIO ((callback . UsrEv) PaneMoveStart)
    return False
  _ <- hpane' `Gtk.on` Gtk.buttonReleaseEvent $ do
    liftIO ((callback . UsrEv) PaneMoveEnd)
    return False
  Gtk.panedPack1 hpane' win1 True False
  Gtk.panedPack2 hpane' win2 True False
  Gtk.widgetShowAll hpane'
  return (uhdl'', Gtk.castToWidget hpane', HSplit wconf1' wconf2')
constructFrame' callback template uhdl (VSplit wconf1 wconf2) = do
  (uhdl', win1, wconf1') <- constructFrame' callback template uhdl wconf1
  (uhdl'', win2, wconf2') <- constructFrame' callback template uhdl' wconf2
  vpane' <- Gtk.vPanedNew
  _ <- vpane' `Gtk.on` Gtk.buttonPressEvent $ do
    liftIO ((callback . UsrEv) PaneMoveStart)
    return False
  _ <- vpane' `Gtk.on` Gtk.buttonReleaseEvent $ do
    liftIO ((callback . UsrEv) PaneMoveEnd)
    return False
  Gtk.panedPack1 vpane' win1 True False
  Gtk.panedPack2 vpane' win2 True False
  Gtk.widgetShowAll vpane'
  return (uhdl'', Gtk.castToWidget vpane', VSplit wconf1' wconf2')

registerFrameToContainer :: Gtk.Window -> Gtk.Box -> Gtk.Widget -> IO ()
registerFrameToContainer rtrwin rtcntr win = do
  Gtk.boxPackEnd rtcntr win Gtk.PackGrow 0
  Gtk.widgetShowAll rtrwin
  Gtk.widgetQueueDraw rtrwin

createTab :: (AllEvent -> IO ()) -> Gtk.Notebook -> Gtk.VBox -> IO (Int, UUID, Gtk.Button)
createTab callback notebook vboxcvs = do
  hbox <- Gtk.hBoxNew False 0
  ebox <- Gtk.eventBoxNew
  label <- Gtk.labelNew (Just "      " :: Maybe String)
  Gtk.containerAdd ebox label
  Gtk.dragSourceSet ebox [Gtk.Button1] [Gtk.ActionCopy]
  Gtk.dragSourceSetIconStock ebox Gtk.stockIndex
  Gtk.dragSourceAddTextTargets ebox
  button <- Gtk.buttonNewWithLabel ("X" :: String)
  Gtk.boxPackStart hbox ebox Gtk.PackNatural 0
  Gtk.boxPackStart hbox button Gtk.PackNatural 0
  Gtk.widgetShowAll hbox
  mlabel <- Gtk.labelNew (Nothing :: Maybe String)
  n <- Gtk.notebookAppendPageMenu notebook vboxcvs hbox mlabel
  uuid <- nextRandom
  _ <- button `Gtk.on` Gtk.buttonActivated $ callback (UsrEv (CloseTab uuid))
  _ <- ebox `Gtk.on` Gtk.dragDataGet $ \_dc _iid _ts -> do
    minfo <- liftIO $ do
      ref <- newIORef (Nothing :: Maybe String)
      callback (UsrEv (GetHoodleFileInfoFromTab uuid ref))
      readIORef ref
    traverse_ Gtk.selectionDataSetText minfo
  return (n, uuid, button)
