{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.HoodleState 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Type.HoodleState 
( HoodleState(..)
, HoodleModeState(..)
, IsOneTimeSelectMode(..)
, Settings(..)
, UIComponentSignalHandler(..)
-- | labels
, hoodleModeState
, hoodleFileControl
, cvsInfoMap
, currentCanvas
, frameState
, rootWindow
, rootContainer
, rootOfRootWindow
, currentPenDraw
, callBack
, deviceList
, penInfo
, selectInfo
, gtkUIManager
, isSaved
, undoTable
, backgroundStyle 
, isFullScreen 
, settings
, uiComponentSignalHandler
, isOneTimeSelectMode
, lastTimeCanvasConfigure
, hookSet 
, tempLog 
, tempQueue 
, statusBar
-- 
, hoodleFileName 
, lastSavedTime
--
, doesUseXInput 
, doesUseTouch
, doesSmoothScroll 
, doesUsePopUpMenu
, doesEmbedImage
, doesEmbedPDF
, doesFollowLinks
-- 
, penModeSignal
, pageModeSignal
, penPointSignal
, penColorSignal
-- | others 
, emptyHoodleState
, defaultSettings 
, defaultUIComponentSignalHandler
, getHoodle
-- | additional lenses 
, getCanvasInfoMap 
, setCanvasInfoMap 
, getCurrentCanvasId
, setCurrentCanvasId
, currentCanvasInfo
, resetHoodleModeStateBuffers
, getCanvasInfo
, setCanvasInfo
, updateFromCanvasInfoAsCurrentCanvas
, setCanvasId
, modifyCanvasInfo
, hoodleModeStateEither
, getCurrentPageFromHoodleModeState
, getCurrentPageDimFromHoodleModeState
-- | for debug
, showCanvasInfoMapViewPortBBox
) where

import           Control.Category
import           Control.Lens
import           Control.Monad.State hiding (get,modify)
import qualified Data.IntMap as M
import           Data.Maybe
import           Data.Time.Clock
import           Graphics.UI.Gtk hiding (Clipboard, get,set)
-- from hoodle-platform
import           Control.Monad.Trans.Crtn.Event 
import           Control.Monad.Trans.Crtn.Queue 
import           Data.Hoodle.Generic
import           Data.Hoodle.Select
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Type
-- from this package 
import           Hoodle.Device
import           Hoodle.Script.Hook
import           Hoodle.Type.Enum 
import           Hoodle.Type.Event 
import           Hoodle.Type.Canvas
import           Hoodle.Type.Window 
import           Hoodle.Type.Undo
import           Hoodle.Type.Alias 
import           Hoodle.Type.PageArrangement
import           Hoodle.Util
-- 
import Prelude hiding ((.), id)

-- | 

data HoodleModeState = ViewAppendState { unView :: RHoodle }
                     | SelectState { tempSelect :: HHoodle }
                    
-- | 

data IsOneTimeSelectMode = NoOneTimeSelectMode 
                         | YesBeforeSelect 
                         | YesAfterSelect
                         deriving (Show,Eq,Ord)

data HoodleState = 
  HoodleState { _hoodleModeState :: HoodleModeState
                , _hoodleFileControl :: HoodleFileControl
                , _cvsInfoMap :: CanvasInfoMap 
                , _currentCanvas :: (CanvasId,CanvasInfoBox)
                , _frameState :: WindowConfig 
                , _rootWindow :: Widget
                , _rootContainer :: Box
                , _rootOfRootWindow :: Window
                , _currentPenDraw :: PenDraw
                , _callBack ::  AllEvent -> IO ()
                , _deviceList :: DeviceList
                , _penInfo :: PenInfo
                , _selectInfo :: SelectInfo 
                , _gtkUIManager :: UIManager 
                , _isSaved :: Bool 
                , _undoTable :: UndoTable HoodleModeState
                , _backgroundStyle :: BackgroundStyle 
                , _isFullScreen :: Bool 
                , _settings :: Settings 
                , _uiComponentSignalHandler :: UIComponentSignalHandler 
                , _isOneTimeSelectMode :: IsOneTimeSelectMode
                , _lastTimeCanvasConfigure :: Maybe UTCTime 
                , _hookSet :: Maybe Hook
                , _tempQueue :: Queue (Either (ActionOrder AllEvent) AllEvent)
                , _tempLog :: String -> String 
                , _statusBar :: Maybe Statusbar
                } 


-- | lens for hoodleModeState
hoodleModeState :: Simple Lens HoodleState HoodleModeState
hoodleModeState = lens _hoodleModeState (\f a -> f { _hoodleModeState = a } )



-- | 
hoodleFileControl :: Simple Lens HoodleState HoodleFileControl
hoodleFileControl = lens _hoodleFileControl (\f a -> f { _hoodleFileControl = a })



-- | lens for cvsInfoMap
cvsInfoMap :: Simple Lens HoodleState CanvasInfoMap
cvsInfoMap = lens _cvsInfoMap (\f a -> f { _cvsInfoMap = a } )

-- | lens for currentCanvas
currentCanvas :: Simple Lens HoodleState (CanvasId,CanvasInfoBox)
currentCanvas = lens _currentCanvas (\f a -> f { _currentCanvas = a } )

-- | lens for frameState
frameState :: Simple Lens HoodleState WindowConfig
frameState = lens _frameState (\f a -> f { _frameState = a } )

-- | lens for rootWindow
rootWindow :: Simple Lens HoodleState Widget
rootWindow = lens _rootWindow (\f a -> f { _rootWindow = a } )

-- | lens for rootContainer
rootContainer :: Simple Lens HoodleState Box
rootContainer = lens _rootContainer (\f a -> f { _rootContainer = a } )

-- | lens for rootOfRootWindow
rootOfRootWindow :: Simple Lens HoodleState Window
rootOfRootWindow = lens _rootOfRootWindow (\f a -> f { _rootOfRootWindow = a } )

-- | lens for currentPenDraw
currentPenDraw :: Simple Lens HoodleState PenDraw
currentPenDraw = lens _currentPenDraw (\f a -> f { _currentPenDraw = a } )

-- | lens for callBack
callBack :: Simple Lens HoodleState (AllEvent -> IO ())
callBack = lens _callBack (\f a -> f { _callBack = a } )

-- | lens for deviceList
deviceList :: Simple Lens HoodleState DeviceList 
deviceList = lens _deviceList (\f a -> f { _deviceList = a } )

-- | lens for penInfo
penInfo :: Simple Lens HoodleState PenInfo
penInfo = lens _penInfo (\f a -> f { _penInfo = a } )

-- | lens for selectInfo
selectInfo :: Simple Lens HoodleState SelectInfo
selectInfo = lens _selectInfo (\f a -> f { _selectInfo = a } )

-- | lens for gtkUIManager
gtkUIManager :: Simple Lens HoodleState UIManager
gtkUIManager = lens _gtkUIManager (\f a -> f { _gtkUIManager = a } )

-- | lens for isSaved
isSaved :: Simple Lens HoodleState Bool
isSaved = lens _isSaved (\f a -> f { _isSaved = a } )

-- | lens for undoTable
undoTable :: Simple Lens HoodleState (UndoTable HoodleModeState)
undoTable = lens _undoTable (\f a -> f { _undoTable = a } )

-- | background style = plain, lined, ruled, graph
backgroundStyle :: Simple Lens HoodleState BackgroundStyle
backgroundStyle = lens _backgroundStyle (\f a -> f { _backgroundStyle = a } )

-- | lens for isFullScreen
isFullScreen :: Simple Lens HoodleState Bool
isFullScreen = lens _isFullScreen (\f a -> f { _isFullScreen = a } )

-- | 
settings :: Simple Lens HoodleState Settings 
settings = lens _settings (\f a -> f { _settings = a } )

-- | 
uiComponentSignalHandler :: Simple Lens HoodleState UIComponentSignalHandler
uiComponentSignalHandler = lens _uiComponentSignalHandler (\f a -> f { _uiComponentSignalHandler = a })

-- | lens for isOneTimeSelectMode
isOneTimeSelectMode :: Simple Lens HoodleState IsOneTimeSelectMode
isOneTimeSelectMode = lens _isOneTimeSelectMode (\f a -> f { _isOneTimeSelectMode = a } )



-- | lens for lastTimeCanvasConfigure
lastTimeCanvasConfigure :: Simple Lens HoodleState (Maybe UTCTime)
lastTimeCanvasConfigure = lens _lastTimeCanvasConfigure (\f a -> f { _lastTimeCanvasConfigure = a } )

-- | lens for hookSet
hookSet :: Simple Lens HoodleState (Maybe Hook)
hookSet = lens _hookSet (\f a -> f { _hookSet = a } )

-- | lens for tempQueue
tempQueue :: Simple Lens HoodleState (Queue (Either (ActionOrder AllEvent) AllEvent))
tempQueue = lens _tempQueue (\f a -> f { _tempQueue = a } )

-- | lens for tempLog
tempLog :: Simple Lens HoodleState (String -> String)
tempLog = lens _tempLog (\f a -> f { _tempLog = a } )

-- | 
statusBar :: Simple Lens HoodleState (Maybe Statusbar)
statusBar = lens _statusBar (\f a -> f { _statusBar = a })

-- | 
data HoodleFileControl = 
  HoodleFileControl { _hoodleFileName :: Maybe FilePath 
                    , _lastSavedTime  :: Maybe UTCTime 
                    } 

-- | lens for currFileName
hoodleFileName :: Simple Lens HoodleFileControl (Maybe FilePath)
hoodleFileName = lens _hoodleFileName (\f a -> f { _hoodleFileName = a } )

-- | lens for last saved time
lastSavedTime :: Simple Lens HoodleFileControl (Maybe UTCTime) 
lastSavedTime = lens _lastSavedTime (\f a -> f { _lastSavedTime = a } )



-- | 
data UIComponentSignalHandler = 
  UIComponentSignalHandler{ _penModeSignal :: Maybe (ConnectId RadioAction)
                          , _pageModeSignal :: Maybe (ConnectId RadioAction)
                          , _penPointSignal :: Maybe (ConnectId RadioAction)
                          , _penColorSignal :: Maybe (ConnectId RadioAction)
                          } 

-- | lens for penModeSignal
penModeSignal :: Simple Lens UIComponentSignalHandler (Maybe (ConnectId RadioAction))
penModeSignal = lens _penModeSignal (\f a -> f { _penModeSignal = a } )

-- | lens for pageModeSignal
pageModeSignal :: Simple Lens UIComponentSignalHandler (Maybe (ConnectId RadioAction))
pageModeSignal = lens _pageModeSignal (\f a -> f { _pageModeSignal = a } )

-- | lens for penPointSignal
penPointSignal :: Simple Lens UIComponentSignalHandler (Maybe (ConnectId RadioAction))
penPointSignal = lens _penPointSignal (\f a -> f { _penPointSignal = a } )

-- | lens for penColorSignal
penColorSignal :: Simple Lens UIComponentSignalHandler (Maybe (ConnectId RadioAction))
penColorSignal = lens _penColorSignal (\f a -> f { _penColorSignal = a } )


-- | A set of Hoodle settings 
data Settings = 
  Settings { _doesUseXInput :: Bool 
           , _doesUseTouch :: Bool 
           , _doesSmoothScroll :: Bool 
           , _doesUsePopUpMenu :: Bool 
           , _doesEmbedImage :: Bool 
           , _doesEmbedPDF :: Bool 
           , _doesFollowLinks :: Bool 
           } 
  

-- | flag for XInput extension (needed for using full power of wacom)
doesUseXInput :: Simple Lens Settings Bool
doesUseXInput = lens _doesUseXInput (\f a -> f { _doesUseXInput = a } )

-- | flag for touch
doesUseTouch :: Simple Lens Settings Bool
doesUseTouch = lens _doesUseTouch (\f a -> f { _doesUseTouch = a } )

-- | flag for smooth scrolling 
doesSmoothScroll :: Simple Lens Settings Bool
doesSmoothScroll = lens _doesSmoothScroll (\f a -> f { _doesSmoothScroll = a } )

-- | flag for using popup menu
doesUsePopUpMenu :: Simple Lens Settings Bool
doesUsePopUpMenu = lens _doesUsePopUpMenu (\f a -> f { _doesUsePopUpMenu = a } )

-- | flag for embedding image as base64 in hdl file 
doesEmbedImage :: Simple Lens Settings Bool
doesEmbedImage = lens _doesEmbedImage (\f a -> f { _doesEmbedImage = a } )

-- | flag for embedding pdf background as base64 in hdl file 
doesEmbedPDF :: Simple Lens Settings Bool
doesEmbedPDF = lens _doesEmbedPDF (\f a -> f { _doesEmbedPDF = a } )

-- | flag for embedding pdf background as base64 in hdl file 
doesFollowLinks :: Simple Lens Settings Bool
doesFollowLinks = lens _doesFollowLinks (\f a -> f { _doesFollowLinks = a } )


-- | default hoodle state 
emptyHoodleState :: IO HoodleState 
emptyHoodleState = do
  hdl <- emptyGHoodle
  return $
    HoodleState  
    { _hoodleModeState = ViewAppendState hdl 
    , _hoodleFileControl = emptyHoodleFileControl 
    -- , _currFileName = Nothing 
    , _cvsInfoMap = error "emptyHoodleState.cvsInfoMap"
    , _currentCanvas = error "emtpyHoodleState.currentCanvas"
    , _frameState = error "emptyHoodleState.frameState" 
    , _rootWindow = error "emtpyHoodleState.rootWindow"
    , _rootContainer = error "emptyHoodleState.rootContainer"
    , _rootOfRootWindow = error "emptyHoodleState.rootOfRootWindow"
    , _currentPenDraw = emptyPenDraw 
    -- , _clipboard = emptyClipboard
    , _callBack = error "emtpyHoodleState.callBack"
    , _deviceList = error "emtpyHoodleState.deviceList"
    , _penInfo = defaultPenInfo 
    , _selectInfo = SelectInfo SelectRectangleWork 
    , _gtkUIManager = error "emptyHoodleState.gtkUIManager"
    , _isSaved = False 
    , _undoTable = emptyUndo 1 
    -- , _isEventBlocked = False 
    , _backgroundStyle = BkgStyleLined
    , _isFullScreen = False
    , _settings = defaultSettings
    , _uiComponentSignalHandler = defaultUIComponentSignalHandler 
    , _isOneTimeSelectMode = NoOneTimeSelectMode
    -- , _pageModeSignal = Nothing
    -- , _penModeSignal = Nothing                        
    , _lastTimeCanvasConfigure = Nothing                      
    , _hookSet = Nothing
    , _tempQueue = emptyQueue
    , _tempLog = id 
    , _statusBar = Nothing 
    }

emptyHoodleFileControl :: HoodleFileControl 
emptyHoodleFileControl = 
  HoodleFileControl { _hoodleFileName = Nothing 
                    , _lastSavedTime = Nothing 
                    } 


defaultUIComponentSignalHandler :: UIComponentSignalHandler
defaultUIComponentSignalHandler = 
  UIComponentSignalHandler{ _penModeSignal = Nothing 
                          , _pageModeSignal = Nothing 
                          , _penPointSignal = Nothing 
                          , _penColorSignal = Nothing 
                          } 


-- | default settings
defaultSettings :: Settings
defaultSettings = 
  Settings 
  { _doesUseXInput = False
  , _doesUseTouch = True
  , _doesSmoothScroll = False
  , _doesUsePopUpMenu = True 
  , _doesEmbedImage = True 
  , _doesEmbedPDF = True 
  , _doesFollowLinks = False
  } 
  

-- | 
getHoodle :: HoodleState -> Hoodle EditMode 
getHoodle = either id gSelect2GHoodle . hoodleModeStateEither . view hoodleModeState 

-- | 
getCurrentCanvasId :: HoodleState -> CanvasId
getCurrentCanvasId = fst . _currentCanvas 
  
-- | 

setCurrentCanvasId :: CanvasId -> HoodleState -> Maybe HoodleState
setCurrentCanvasId a f = do 
    cinfobox <- M.lookup a (_cvsInfoMap f)
    return (f { _currentCanvas = (a,cinfobox) })
     
-- | 
    
getCanvasInfoMap :: HoodleState -> CanvasInfoMap 
getCanvasInfoMap = _cvsInfoMap 

-- | 

setCanvasInfoMap :: CanvasInfoMap -> HoodleState -> Maybe HoodleState 
setCanvasInfoMap cmap xstate 
  | M.null cmap = Nothing
  | otherwise = 
      let (cid,_) = _currentCanvas xstate
          (cidmax,cinfomax) = M.findMax cmap
          mcinfobox = M.lookup cid cmap 
      in Just . maybe (xstate {_currentCanvas=(cidmax,cinfomax), _cvsInfoMap = cmap}) 
                       (\cinfobox -> xstate {_currentCanvas = (cid,cinfobox)
                                            ,_cvsInfoMap = cmap }) 
                $ mcinfobox

currentCanvasInfo :: Simple Lens HoodleState CanvasInfoBox
currentCanvasInfo = lens getter setter 
  where 
    getter = snd . _currentCanvas 
    setter f a =  
      let cid = fst . _currentCanvas $ f 
          cmap' = M.adjust (const a) cid (_cvsInfoMap f)
      in f { _currentCanvas = (cid,a), _cvsInfoMap = cmap' }

-- | 
resetHoodleModeStateBuffers :: HoodleModeState -> IO HoodleModeState 
resetHoodleModeStateBuffers hdlmodestate1 = 
  case hdlmodestate1 of 
    ViewAppendState hdl -> liftIO . liftM ViewAppendState . updateHoodleBuf $ hdl
    _ -> return hdlmodestate1

-- |
getCanvasInfo :: CanvasId -> HoodleState -> CanvasInfoBox 
getCanvasInfo cid xstate = 
  let cinfoMap = getCanvasInfoMap xstate
      maybeCvs = M.lookup cid cinfoMap
  in maybeError' ("no canvas with id = " ++ show cid) maybeCvs

-- | 
setCanvasInfo :: (CanvasId,CanvasInfoBox) -> HoodleState -> HoodleState 
setCanvasInfo (cid,cinfobox) xstate = 
  let cmap = getCanvasInfoMap xstate
      cmap' = M.insert cid cinfobox cmap 
  in maybe xstate id $ setCanvasInfoMap cmap' xstate



-- | change current canvas. this is the master function  
updateFromCanvasInfoAsCurrentCanvas :: CanvasInfoBox -> HoodleState -> HoodleState
updateFromCanvasInfoAsCurrentCanvas cinfobox xstate = 
  let cid = unboxGet canvasId cinfobox 
      cmap = getCanvasInfoMap xstate
      cmap' = M.insert cid cinfobox cmap 
  in xstate { _currentCanvas = (cid,cinfobox)
            , _cvsInfoMap = cmap' }

-- | 
setCanvasId :: CanvasId -> CanvasInfoBox -> CanvasInfoBox 
setCanvasId cid = insideAction4CvsInfoBox (set canvasId cid) 

-- | 
modifyCanvasInfo :: CanvasId -> (CanvasInfoBox -> CanvasInfoBox) -> HoodleState
                    -> HoodleState
modifyCanvasInfo cid f xstate =  
    maybe xstate id . flip setCanvasInfoMap xstate 
                    . M.adjust f cid . getCanvasInfoMap 
    $ xstate 

-- | 
hoodleModeStateEither :: HoodleModeState -> Either (Hoodle EditMode) (Hoodle SelectMode) 
hoodleModeStateEither hdlmodst = case hdlmodst of 
                            ViewAppendState hdl -> Left hdl
                            SelectState thdl -> Right thdl 

-- | 
getCurrentPageFromHoodleModeState :: (ViewMode a) => CanvasInfo a 
                              -> HoodleModeState -> Page EditMode 
getCurrentPageFromHoodleModeState cinfo hdlmodst = 
  let cpn = view currentPageNum cinfo 
      pagemap = getPageMapFromHoodleModeState hdlmodst   
  in maybeError' "updatePageFromCanvasToHoodle" $ M.lookup cpn pagemap 

-- | 
getCurrentPageDimFromHoodleModeState :: (ViewMode a) => CanvasInfo a 
                              -> HoodleModeState -> PageDimension
getCurrentPageDimFromHoodleModeState cinfo =                               
  PageDimension . view gdimension . getCurrentPageFromHoodleModeState cinfo


-- | 
getPageMapFromHoodleModeState :: HoodleModeState -> M.IntMap (Page EditMode)
getPageMapFromHoodleModeState = either (view gpages) (view gselAll) . hoodleModeStateEither 
  
      
-- | 
showCanvasInfoMapViewPortBBox :: HoodleState -> IO ()
showCanvasInfoMapViewPortBBox xstate = do 
  let cmap = getCanvasInfoMap xstate
  print . map (unboxGet (viewInfo.pageArrangement.viewPortBBox)) . M.elems $ cmap 




