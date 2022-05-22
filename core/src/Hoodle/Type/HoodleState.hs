{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Hoodle.Type.HoodleState
  ( HoodleState,
    HoodleModeState (..),
    UnitHoodle,
    IsOneTimeSelectMode (..),
    Settings,
    UIComponentSignalHandler,
    FileStore (..),
    -- | labels
    unitKey,
    unitUUID,
    unitButton,
    hoodleModeState,
    hoodleFileControl,
    cvsInfoMap,
    currentCanvas,
    isOneTimeSelectMode,
    frameState,
    rootWindow,
    unitHoodles,
    rootNotebook,
    rootContainer,
    rootOfRootWindow,
    currentPenDraw,
    callBack,
    deviceList,
    penInfo,
    cursorInfo,
    selectInfo,
    gtkUIManager,
    isSaved,
    undoTable,
    backgroundStyle,
    isFullScreen,
    settings,
    uiComponentSignalHandler,
    lastTimeCanvasConfigure,
    hookSet,
    tempLog,
    tempQueue,
    statusBar,
    renderCacheVar,
    pdfRenderQueue,
    genRenderQueue,
    doesNotInvalidate,
    nextPdfBkgPageNum,
    --
    hoodleFileName,
    lastSavedTime,
    syncMD5History,
    --
    doesUseXInput,
    doesUseTouch,
    doesUsePopUpMenu,
    doesEmbedImage,
    doesEmbedPDF,
    doesFollowLinks,
    doesKeepAspectRatio,
    doesUseVariableCursor,
    newPageMode,
    networkEditSourceInfo,
    --
    penModeSignal,
    pageModeSignal,
    penPointSignal,
    penColorSignal,
    newPageModeSignal,
    switchTabSignal,
    -- | others
    emptyUnitHoodle,
    emptyHoodleState,
    defaultSettings,
    defaultUIComponentSignalHandler,
    getHoodle,
    -- | additional lenses
    --  , getCanvasInfoMap
    setCanvasInfoMap,
    getCurrentCanvasId,
    setCurrentCanvasId,
    currentCanvasInfo,
    resetHoodleModeStateBuffers,
    getCanvasInfo,
    setCanvasInfo,
    updateFromCanvasInfoAsCurrentCanvas,
    setCanvasId,
    modifyCanvasInfo,
    hoodleModeStateEither,
    getCurrentPageFromHoodleModeState,
    getCurrentPageDimFromHoodleModeState,
    -- | for debug
    -- , showCanvasInfoMapViewPortBBox
    -- , getTheUnit
    -- , putTheUnit
    currentUnit,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens (Lens', lens, set, view, (^.))
import Control.Monad.Trans.Crtn.Event
import Control.Monad.Trans.Crtn.Queue
import Data.Functor.Identity (Identity (..))
import qualified Data.HashMap.Strict as HM
import Data.Hoodle.Generic
import Data.Hoodle.Select
import qualified Data.IntMap as M
import Data.Sequence
import qualified Data.Text as T
import Data.Time.Clock
import Data.UUID (UUID)
import Graphics.Hoodle.Render
import Graphics.Hoodle.Render.Type
import qualified Graphics.UI.Gtk as Gtk hiding (Clipboard, get, set)
import Hoodle.Device
import Hoodle.Script.Hook
import Hoodle.Type.Alias
import Hoodle.Type.Canvas
import Hoodle.Type.Enum
import Hoodle.Type.Event
import Hoodle.Type.PageArrangement
import Hoodle.Type.Undo
import Hoodle.Type.Window
import Hoodle.Util

-- |
data HoodleModeState
  = ViewAppendState {unView :: RHoodle}
  | SelectState {tempSelect :: HHoodle}

-- |
data IsOneTimeSelectMode
  = NoOneTimeSelectMode
  | YesBeforeSelect
  | YesAfterSelect
  deriving (Show, Eq, Ord)

data UnitHoodle = UnitHoodle
  { _unitKey :: Int,
    _unitUUID :: UUID,
    _unitButton :: Gtk.Button,
    _hoodleModeState :: HoodleModeState,
    _hoodleFileControl :: HoodleFileControl,
    _cvsInfoMap :: CanvasInfoMap,
    _currentCanvas :: (CanvasId, CanvasInfoBox),
    _frameState :: WindowConfig,
    _rootWindow :: Gtk.Widget,
    _rootContainer :: Gtk.Box,
    _isSaved :: Bool,
    _undoTable :: UndoTable HoodleModeState,
    _isOneTimeSelectMode :: IsOneTimeSelectMode
  }

data HoodleState = HoodleState
  { _unitHoodles :: (Int, M.IntMap UnitHoodle),
    _rootNotebook :: Gtk.Notebook,
    _rootOfRootWindow :: Gtk.Window,
    _currentPenDraw :: PenDraw,
    _callBack :: AllEvent -> IO (),
    _deviceList :: DeviceList,
    _penInfo :: PenInfo,
    -- | (pen color, pen width, use variable cursor)
    _cursorInfo :: (PenColor, Double, Bool),
    _selectInfo :: SelectInfo,
    _gtkUIManager :: Gtk.UIManager,
    _isFullScreen :: Bool,
    _settings :: Settings,
    _backgroundStyle :: BackgroundStyle,
    _uiComponentSignalHandler :: UIComponentSignalHandler,
    _lastTimeCanvasConfigure :: Maybe UTCTime,
    _hookSet :: Maybe Hook,
    _tempQueue :: Queue (Either (ActionOrder AllEvent) AllEvent),
    _tempLog :: String -> String,
    _statusBar :: Maybe Gtk.Statusbar,
    _renderCacheVar :: TVar RenderCache,
    _pdfRenderQueue :: PDFCommandQueue,
    _genRenderQueue :: GenCommandQueue,
    _doesNotInvalidate :: Bool,
    _nextPdfBkgPageNum :: Maybe Int
  }

-- | current unit
currentUnit :: Lens' (Int, M.IntMap UnitHoodle) UnitHoodle
currentUnit = lens (\(k, m) -> fromJustError "currentUnit" (M.lookup k m)) (\(_, m) a -> (_unitKey a, M.insert (_unitKey a) a m))

-- | lens for unitKey
unitKey :: Lens' UnitHoodle Int
unitKey = lens _unitKey (\f a -> f {_unitKey = a})

-- | lens for unitKey
unitUUID :: Lens' UnitHoodle UUID
unitUUID = lens _unitUUID (\f a -> f {_unitUUID = a})

-- | lens for unitKey
unitButton :: Lens' UnitHoodle Gtk.Button
unitButton = lens _unitButton (\f a -> f {_unitButton = a})

-- | lens for hoodleModeState
hoodleModeState :: Lens' UnitHoodle HoodleModeState
hoodleModeState = lens _hoodleModeState (\f a -> f {_hoodleModeState = a})

-- |
hoodleFileControl :: Lens' UnitHoodle HoodleFileControl
hoodleFileControl = lens _hoodleFileControl (\f a -> f {_hoodleFileControl = a})

-- | lens for cvsInfoMap
cvsInfoMap :: Lens' UnitHoodle CanvasInfoMap
cvsInfoMap = lens _cvsInfoMap (\f a -> f {_cvsInfoMap = a})

-- | lens for currentCanvas
currentCanvas :: Lens' UnitHoodle (CanvasId, CanvasInfoBox)
currentCanvas = lens _currentCanvas (\f a -> f {_currentCanvas = a})

-- | lens for frameState
frameState :: Lens' UnitHoodle WindowConfig
frameState = lens _frameState (\f a -> f {_frameState = a})

-- | lens for rootWindow
rootWindow :: Lens' UnitHoodle Gtk.Widget
rootWindow = lens _rootWindow (\f a -> f {_rootWindow = a})

-- | lens for rootContainer
rootContainer :: Lens' UnitHoodle Gtk.Box
rootContainer = lens _rootContainer (\f a -> f {_rootContainer = a})

-- | lens for isSaved
isSaved :: Lens' UnitHoodle Bool
isSaved = lens _isSaved (\f a -> f {_isSaved = a})

-- | lens for undoTable
undoTable :: Lens' UnitHoodle (UndoTable HoodleModeState)
undoTable = lens _undoTable (\f a -> f {_undoTable = a})

-- | lens for isOneTimeSelectMode
isOneTimeSelectMode :: Lens' UnitHoodle IsOneTimeSelectMode
isOneTimeSelectMode = lens _isOneTimeSelectMode (\f a -> f {_isOneTimeSelectMode = a})

-- | lens for unitHoodles
unitHoodles :: Lens' HoodleState (Int, M.IntMap UnitHoodle)
unitHoodles = lens _unitHoodles (\f a -> f {_unitHoodles = a})

-- | lens for rootWindow
rootNotebook :: Lens' HoodleState Gtk.Notebook
rootNotebook = lens _rootNotebook (\f a -> f {_rootNotebook = a})

-- | lens for rootOfRootWindow
rootOfRootWindow :: Lens' HoodleState Gtk.Window
rootOfRootWindow = lens _rootOfRootWindow (\f a -> f {_rootOfRootWindow = a})

-- | lens for currentPenDraw
currentPenDraw :: Lens' HoodleState PenDraw
currentPenDraw = lens _currentPenDraw (\f a -> f {_currentPenDraw = a})

-- | lens for callBack
callBack :: Lens' HoodleState (AllEvent -> IO ())
callBack = lens _callBack (\f a -> f {_callBack = a})

-- | lens for deviceList
deviceList :: Lens' HoodleState DeviceList
deviceList = lens _deviceList (\f a -> f {_deviceList = a})

-- | lens for penInfo
penInfo :: Lens' HoodleState PenInfo
penInfo = lens _penInfo (\f a -> f {_penInfo = a})

-- | lens for cursorInfo
cursorInfo :: Lens' HoodleState (PenColor, Double, Bool)
cursorInfo = lens _cursorInfo (\f a -> f {_cursorInfo = a})

-- | lens for selectInfo
selectInfo :: Lens' HoodleState SelectInfo
selectInfo = lens _selectInfo (\f a -> f {_selectInfo = a})

-- | lens for gtkUIManager
gtkUIManager :: Lens' HoodleState Gtk.UIManager
gtkUIManager = lens _gtkUIManager (\f a -> f {_gtkUIManager = a})

-- | background style = plain, lined, ruled, graph
backgroundStyle :: Lens' HoodleState BackgroundStyle
backgroundStyle = lens _backgroundStyle (\f a -> f {_backgroundStyle = a})

-- | lens for isFullScreen
isFullScreen :: Lens' HoodleState Bool
isFullScreen = lens _isFullScreen (\f a -> f {_isFullScreen = a})

-- |
settings :: Lens' HoodleState Settings
settings = lens _settings (\f a -> f {_settings = a})

-- |
uiComponentSignalHandler :: Lens' HoodleState UIComponentSignalHandler
uiComponentSignalHandler = lens _uiComponentSignalHandler (\f a -> f {_uiComponentSignalHandler = a})

-- | lens for lastTimeCanvasConfigure
lastTimeCanvasConfigure :: Lens' HoodleState (Maybe UTCTime)
lastTimeCanvasConfigure = lens _lastTimeCanvasConfigure (\f a -> f {_lastTimeCanvasConfigure = a})

-- | lens for hookSet
hookSet :: Lens' HoodleState (Maybe Hook)
hookSet = lens _hookSet (\f a -> f {_hookSet = a})

-- | lens for tempQueue
tempQueue :: Lens' HoodleState (Queue (Either (ActionOrder AllEvent) AllEvent))
tempQueue = lens _tempQueue (\f a -> f {_tempQueue = a})

-- | lens for tempLog
tempLog :: Lens' HoodleState (String -> String)
tempLog = lens _tempLog (\f a -> f {_tempLog = a})

-- |
statusBar :: Lens' HoodleState (Maybe Gtk.Statusbar)
statusBar = lens _statusBar (\f a -> f {_statusBar = a})

-- |
renderCacheVar :: Lens' HoodleState (TVar RenderCache)
renderCacheVar = lens _renderCacheVar (\f a -> f {_renderCacheVar = a})

-- |
pdfRenderQueue :: Lens' HoodleState PDFCommandQueue
pdfRenderQueue = lens _pdfRenderQueue (\f a -> f {_pdfRenderQueue = a})

-- |
genRenderQueue :: Lens' HoodleState GenCommandQueue
genRenderQueue = lens _genRenderQueue (\f a -> f {_genRenderQueue = a})

-- |
doesNotInvalidate :: Lens' HoodleState Bool
doesNotInvalidate = lens _doesNotInvalidate (\f a -> f {_doesNotInvalidate = a})

-- |
nextPdfBkgPageNum :: Lens' HoodleState (Maybe Int)
nextPdfBkgPageNum = lens _nextPdfBkgPageNum (\f a -> f {_nextPdfBkgPageNum = a})

{-
-- |
cursorInfo :: Lens' HoodleState (Maybe Cursor)
cursorInfo = lens _cursorInfo (\f a -> f { _cursorInfo = a })
-}

data FileStore
  = LocalDir (Maybe FilePath)
  | TempDir FilePath

-- |
data HoodleFileControl = HoodleFileControl
  { _hoodleFileName :: FileStore, -- Maybe FilePath
    _lastSavedTime :: Maybe UTCTime,
    _syncMD5History :: [T.Text]
  }

-- | lens for currFileName
hoodleFileName :: Lens' HoodleFileControl FileStore -- (Maybe FilePath)
hoodleFileName = lens _hoodleFileName (\f a -> f {_hoodleFileName = a})

-- | lens for last saved time
lastSavedTime :: Lens' HoodleFileControl (Maybe UTCTime)
lastSavedTime = lens _lastSavedTime (\f a -> f {_lastSavedTime = a})

-- | lens for last saved time
syncMD5History :: Lens' HoodleFileControl [T.Text]
syncMD5History = lens _syncMD5History (\f a -> f {_syncMD5History = a})

-- |
data UIComponentSignalHandler = UIComponentSignalHandler
  { _penModeSignal :: Maybe (Gtk.ConnectId Gtk.RadioAction),
    _pageModeSignal :: Maybe (Gtk.ConnectId Gtk.RadioAction),
    _penPointSignal :: Maybe (Gtk.ConnectId Gtk.RadioAction),
    _penColorSignal :: Maybe (Gtk.ConnectId Gtk.RadioAction),
    _newPageModeSignal :: Maybe (Gtk.ConnectId Gtk.RadioAction),
    _switchTabSignal :: Maybe (Gtk.ConnectId Gtk.Notebook)
  }

-- | lens for penModeSignal
penModeSignal :: Lens' UIComponentSignalHandler (Maybe (Gtk.ConnectId Gtk.RadioAction))
penModeSignal = lens _penModeSignal (\f a -> f {_penModeSignal = a})

-- | lens for pageModeSignal
pageModeSignal :: Lens' UIComponentSignalHandler (Maybe (Gtk.ConnectId Gtk.RadioAction))
pageModeSignal = lens _pageModeSignal (\f a -> f {_pageModeSignal = a})

-- | lens for penPointSignal
penPointSignal :: Lens' UIComponentSignalHandler (Maybe (Gtk.ConnectId Gtk.RadioAction))
penPointSignal = lens _penPointSignal (\f a -> f {_penPointSignal = a})

-- | lens for penColorSignal
penColorSignal :: Lens' UIComponentSignalHandler (Maybe (Gtk.ConnectId Gtk.RadioAction))
penColorSignal = lens _penColorSignal (\f a -> f {_penColorSignal = a})

-- | lens for newPageModeSignal
newPageModeSignal :: Lens' UIComponentSignalHandler (Maybe (Gtk.ConnectId Gtk.RadioAction))
newPageModeSignal = lens _newPageModeSignal (\f a -> f {_newPageModeSignal = a})

-- | lens for switchTabSignal
switchTabSignal :: Lens' UIComponentSignalHandler (Maybe (Gtk.ConnectId Gtk.Notebook))
switchTabSignal = lens _switchTabSignal (\f a -> f {_switchTabSignal = a})

-- | A set of Hoodle settings
data Settings = Settings
  { _doesUseXInput :: Bool,
    _doesUseTouch :: Bool,
    _doesUsePopUpMenu :: Bool,
    _doesEmbedImage :: Bool,
    _doesEmbedPDF :: Bool,
    _doesFollowLinks :: Bool,
    _doesKeepAspectRatio :: Bool,
    _doesUseVariableCursor :: Bool,
    _newPageMode :: NewPageModeType,
    _networkEditSourceInfo :: Maybe ThreadId
  }

-- | flag for XInput extension (needed for using full power of wacom)
doesUseXInput :: Lens' Settings Bool
doesUseXInput = lens _doesUseXInput (\f a -> f {_doesUseXInput = a})

-- | flag for touch
doesUseTouch :: Lens' Settings Bool
doesUseTouch = lens _doesUseTouch (\f a -> f {_doesUseTouch = a})

-- | flag for using popup menu
doesUsePopUpMenu :: Lens' Settings Bool
doesUsePopUpMenu = lens _doesUsePopUpMenu (\f a -> f {_doesUsePopUpMenu = a})

-- | flag for embedding image as base64 in hdl file
doesEmbedImage :: Lens' Settings Bool
doesEmbedImage = lens _doesEmbedImage (\f a -> f {_doesEmbedImage = a})

-- | flag for embedding pdf background as base64 in hdl file
doesEmbedPDF :: Lens' Settings Bool
doesEmbedPDF = lens _doesEmbedPDF (\f a -> f {_doesEmbedPDF = a})

-- | flag for embedding pdf background as base64 in hdl file
doesFollowLinks :: Lens' Settings Bool
doesFollowLinks = lens _doesFollowLinks (\f a -> f {_doesFollowLinks = a})

-- | flag for keeping aspect ratio
doesKeepAspectRatio :: Lens' Settings Bool
doesKeepAspectRatio = lens _doesKeepAspectRatio (\f a -> f {_doesKeepAspectRatio = a})

-- | flag for variable cursor
doesUseVariableCursor :: Lens' Settings Bool
doesUseVariableCursor = lens _doesUseVariableCursor (\f a -> f {_doesUseVariableCursor = a})

-- | new page mode: plain | last | cycle
newPageMode :: Lens' Settings NewPageModeType
newPageMode = lens _newPageMode (\f a -> f {_newPageMode = a})

-- | network edit source mode
networkEditSourceInfo :: Lens' Settings (Maybe ThreadId)
networkEditSourceInfo = lens _networkEditSourceInfo (\f a -> f {_networkEditSourceInfo = a})

-- |
emptyUnitHoodle :: IO UnitHoodle
emptyUnitHoodle = do
  hdl <- emptyGHoodle
  return $
    UnitHoodle
      { _unitKey = 0,
        _unitUUID = error "unitUUID",
        _unitButton = error "unitButton",
        _hoodleModeState = ViewAppendState hdl,
        _hoodleFileControl = emptyHoodleFileControl,
        _cvsInfoMap = error "emptyHoodleState.cvsInfoMap",
        _currentCanvas = error "emtpyHoodleState.currentCanvas",
        _frameState = error "emptyHoodleState.frameState",
        _rootWindow = error "emtpyHoodleState.rootWindow",
        _rootContainer = error "emptyHoodleState.rootContainer",
        _isSaved = False,
        _undoTable = emptyUndo 1,
        _isOneTimeSelectMode = NoOneTimeSelectMode
      }

-- | default hoodle state
emptyHoodleState :: IO HoodleState
emptyHoodleState = do
  unit <- emptyUnitHoodle
  tvarpdf <- atomically $ newTVar empty
  tvargen <- atomically $ newTVar empty
  tvarcache <- atomically $ newTVar HM.empty
  return $
    HoodleState
      { _unitHoodles = (0, M.singleton 0 unit),
        _rootNotebook = error "emtpyHoodleState.rootNotebook",
        _rootOfRootWindow = error "emptyHoodleState.rootOfRootWindow",
        _currentPenDraw = emptyPenDraw,
        _callBack = error "emtpyHoodleState.callBack",
        _deviceList = error "emtpyHoodleState.deviceList",
        _penInfo = defaultPenInfo,
        _cursorInfo = (defaultPenInfo ^. penSet . currPen . penColor, defaultPenInfo ^. penSet . currPen . penWidth, False),
        _selectInfo = SelectInfo SelectLassoWork,
        _gtkUIManager = error "emptyHoodleState.gtkUIManager",
        -- , _isEventBlocked = False
        _backgroundStyle = BkgStyleLined,
        _isFullScreen = False,
        _settings = defaultSettings,
        _uiComponentSignalHandler = defaultUIComponentSignalHandler,
        -- , _pageModeSignal = Nothing
        -- , _penModeSignal = Nothing
        _lastTimeCanvasConfigure = Nothing,
        _hookSet = Nothing,
        _tempQueue = emptyQueue,
        _tempLog = id,
        _statusBar = Nothing,
        _renderCacheVar = tvarcache, -- HM.empty
        _pdfRenderQueue = tvarpdf,
        _genRenderQueue = tvargen,
        _doesNotInvalidate = False,
        _nextPdfBkgPageNum = Nothing
        -- , _cursorInfo = Nothing
      }

emptyHoodleFileControl :: HoodleFileControl
emptyHoodleFileControl =
  HoodleFileControl
    { _hoodleFileName = LocalDir Nothing,
      _lastSavedTime = Nothing,
      _syncMD5History = []
    }

defaultUIComponentSignalHandler :: UIComponentSignalHandler
defaultUIComponentSignalHandler =
  UIComponentSignalHandler
    { _penModeSignal = Nothing,
      _pageModeSignal = Nothing,
      _penPointSignal = Nothing,
      _penColorSignal = Nothing,
      _newPageModeSignal = Nothing,
      _switchTabSignal = Nothing
    }

-- | default settings
defaultSettings :: Settings
defaultSettings =
  Settings
    { _doesUseXInput = False,
      _doesUseTouch = True,
      _doesUsePopUpMenu = True,
      _doesEmbedImage = True,
      _doesEmbedPDF = True,
      _doesFollowLinks = True,
      _doesKeepAspectRatio = False,
      _doesUseVariableCursor = False,
      _newPageMode = NPPlain,
      _networkEditSourceInfo = Nothing
    }

-- |
getHoodle :: UnitHoodle -> Hoodle EditMode
getHoodle = either id gSelect2GHoodle . hoodleModeStateEither . view hoodleModeState

-- |
getCurrentCanvasId :: UnitHoodle -> CanvasId
getCurrentCanvasId = fst . _currentCanvas

-- |
setCurrentCanvasId :: CanvasId -> UnitHoodle -> Maybe UnitHoodle
setCurrentCanvasId a f = do
  cinfobox <- M.lookup a (_cvsInfoMap f)
  return (f {_currentCanvas = (a, cinfobox)})

-- |
setCanvasInfoMap :: CanvasInfoMap -> UnitHoodle -> Maybe UnitHoodle
setCanvasInfoMap cmap uhdl
  | M.null cmap = Nothing
  | otherwise =
    let (cid, _) = _currentCanvas uhdl
        (cidmax, cinfomax) = M.findMax cmap
        mcinfobox = M.lookup cid cmap
     in Just
          . maybe
            (uhdl {_currentCanvas = (cidmax, cinfomax), _cvsInfoMap = cmap})
            ( \cinfobox ->
                uhdl
                  { _currentCanvas = (cid, cinfobox),
                    _cvsInfoMap = cmap
                  }
            )
          $ mcinfobox

currentCanvasInfo :: Lens' UnitHoodle CanvasInfoBox
currentCanvasInfo = lens getter setter
  where
    getter = snd . _currentCanvas
    setter f a =
      let cid = fst . _currentCanvas $ f
          cmap' = M.adjust (const a) cid (_cvsInfoMap f)
       in f {_currentCanvas = (cid, a), _cvsInfoMap = cmap'}

-- |
resetHoodleModeStateBuffers :: CanvasId -> HoodleModeState -> Renderer ()
resetHoodleModeStateBuffers cid hdlmodestate1 =
  case hdlmodestate1 of
    ViewAppendState hdl -> updateHoodleBuf cid hdl
    _ -> return ()

-- |
getCanvasInfo :: CanvasId -> UnitHoodle -> CanvasInfoBox
getCanvasInfo cid uhdl =
  let cinfoMap = view cvsInfoMap uhdl
      maybeCvs = M.lookup cid cinfoMap
   in maybeError' ("no canvas with id = " ++ show cid) maybeCvs

-- |
setCanvasInfo :: (CanvasId, CanvasInfoBox) -> UnitHoodle -> UnitHoodle
setCanvasInfo (cid, cinfobox) uhdl =
  let cmap = view cvsInfoMap uhdl
      cmap' = M.insert cid cinfobox cmap
   in maybe uhdl id $ setCanvasInfoMap cmap' uhdl

-- | change current canvas. this is the master function
updateFromCanvasInfoAsCurrentCanvas :: CanvasInfoBox -> UnitHoodle -> UnitHoodle
updateFromCanvasInfoAsCurrentCanvas cinfobox uhdl =
  let cid = view (unboxLens canvasId) cinfobox
      cmap = view cvsInfoMap uhdl
      cmap' = M.insert cid cinfobox cmap
   in uhdl
        { _currentCanvas = (cid, cinfobox),
          _cvsInfoMap = cmap'
        }

-- |
setCanvasId :: CanvasId -> CanvasInfoBox -> CanvasInfoBox
setCanvasId cid = runIdentity . forBoth unboxBiXform (return . set canvasId cid)

-- |
modifyCanvasInfo ::
  CanvasId ->
  (CanvasInfoBox -> CanvasInfoBox) ->
  UnitHoodle ->
  UnitHoodle
modifyCanvasInfo cid f uhdl =
  maybe uhdl id . flip setCanvasInfoMap uhdl
    . M.adjust f cid
    . view cvsInfoMap
    $ uhdl

-- |
hoodleModeStateEither :: HoodleModeState -> Either (Hoodle EditMode) (Hoodle SelectMode)
hoodleModeStateEither hdlmodst = case hdlmodst of
  ViewAppendState hdl -> Left hdl
  SelectState thdl -> Right thdl

-- |
getCurrentPageFromHoodleModeState ::
  CanvasInfo a -> HoodleModeState -> Page EditMode
getCurrentPageFromHoodleModeState cinfo hdlmodst =
  let cpn = view currentPageNum cinfo
      pagemap = getPageMapFromHoodleModeState hdlmodst
   in maybeError' "updatePageFromCanvasToHoodle" $ M.lookup cpn pagemap

-- |
getCurrentPageDimFromHoodleModeState ::
  CanvasInfo a -> HoodleModeState -> PageDimension
getCurrentPageDimFromHoodleModeState cinfo =
  PageDimension . view gdimension . getCurrentPageFromHoodleModeState cinfo

-- |
getPageMapFromHoodleModeState :: HoodleModeState -> M.IntMap (Page EditMode)
getPageMapFromHoodleModeState = either (view gpages) (view gselAll) . hoodleModeStateEither
