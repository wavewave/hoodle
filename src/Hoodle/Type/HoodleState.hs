{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.HoodleState 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
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
-- | labels
, hoodleModeState
, currFileName
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
, isOneTimeSelectMode
, pageModeSignal
, lastTimeCanvasConfigure
, hookSet 
, tempLog 
, tempQueue 
, doesUseXInput 
, doesSmoothScroll 
, doesUsePopUpMenu
, doesEmbedImage
-- | others 
, emptyHoodleState
, defaultSettings 
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
-- , modifyCurrentCanvasInfo
-- , modifyCurrCvsInfoM
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
-- import           Data.Hoodle.Map
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Type
-- from this package 
import           Hoodle.Device
import           Hoodle.Script.Hook
import           Hoodle.Type.Enum 
import           Hoodle.Type.Event 
import           Hoodle.Type.Canvas
import           Hoodle.Type.Clipboard
import           Hoodle.Type.Window 
import           Hoodle.Type.Undo
import           Hoodle.Type.Alias 
import           Hoodle.Type.PageArrangement
import           Hoodle.Util
-- 
import Prelude hiding ((.), id)


-- type HoodleModeStateIO = StateT HoodleState IO 

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
                , _currFileName :: Maybe FilePath
                , _cvsInfoMap :: CanvasInfoMap 
                , _currentCanvas :: (CanvasId,CanvasInfoBox)
                , _frameState :: WindowConfig 
                , _rootWindow :: Widget
                , _rootContainer :: Box
                , _rootOfRootWindow :: Window
                , _currentPenDraw :: PenDraw
                , _callBack ::  MyEvent -> IO ()
                , _deviceList :: DeviceList
                , _penInfo :: PenInfo
                , _selectInfo :: SelectInfo 
                , _gtkUIManager :: UIManager 
                , _isSaved :: Bool 
                , _undoTable :: UndoTable HoodleModeState
                , _backgroundStyle :: BackgroundStyle 
                , _isFullScreen :: Bool 
                , _settings :: Settings 
                , _isOneTimeSelectMode :: IsOneTimeSelectMode
                , _pageModeSignal :: Maybe (ConnectId RadioAction)
                , _lastTimeCanvasConfigure :: Maybe UTCTime 
                , _hookSet :: Maybe Hook
                , _tempQueue :: Queue (Either (ActionOrder MyEvent) MyEvent)
                , _tempLog :: String -> String 
                } 


-- | lens for hoodleModeState
hoodleModeState :: Simple Lens HoodleState HoodleModeState
hoodleModeState = lens _hoodleModeState (\f a -> f { _hoodleModeState = a } )

-- | lens for currFileName
currFileName :: Simple Lens HoodleState (Maybe FilePath)
currFileName = lens _currFileName (\f a -> f { _currFileName = a } )

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
callBack :: Simple Lens HoodleState (MyEvent -> IO ())
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

-- | lens for isOneTimeSelectMode
isOneTimeSelectMode :: Simple Lens HoodleState IsOneTimeSelectMode
isOneTimeSelectMode = lens _isOneTimeSelectMode (\f a -> f { _isOneTimeSelectMode = a } )

-- | lens for pageModeSignal
pageModeSignal :: Simple Lens HoodleState (Maybe (ConnectId RadioAction))
pageModeSignal = lens _pageModeSignal (\f a -> f { _pageModeSignal = a } )

-- | lens for lastTimeCanvasConfigure
lastTimeCanvasConfigure :: Simple Lens HoodleState (Maybe UTCTime)
lastTimeCanvasConfigure = lens _lastTimeCanvasConfigure (\f a -> f { _lastTimeCanvasConfigure = a } )

-- | lens for hookSet
hookSet :: Simple Lens HoodleState (Maybe Hook)
hookSet = lens _hookSet (\f a -> f { _hookSet = a } )

-- | lens for tempQueue
tempQueue :: Simple Lens HoodleState (Queue (Either (ActionOrder MyEvent) MyEvent))
tempQueue = lens _tempQueue (\f a -> f { _tempQueue = a } )

-- | lens for tempLog
tempLog :: Simple Lens HoodleState (String -> String)
tempLog = lens _tempLog (\f a -> f { _tempLog = a } )


-- | A set of Hoodle settings 
data Settings = 
  Settings { _doesUseXInput :: Bool 
           , _doesSmoothScroll :: Bool 
           , _doesUsePopUpMenu :: Bool 
           , _doesEmbedImage :: Bool 
           } 

-- | flag for XInput extension (needed for using full power of wacom)
doesUseXInput :: Simple Lens Settings Bool
doesUseXInput = lens _doesUseXInput (\f a -> f { _doesUseXInput = a } )

-- | flag for smooth scrolling 
doesSmoothScroll :: Simple Lens Settings Bool
doesSmoothScroll = lens _doesSmoothScroll (\f a -> f { _doesSmoothScroll = a } )

-- | flag for using popup menu
doesUsePopUpMenu :: Simple Lens Settings Bool
doesUsePopUpMenu = lens _doesUsePopUpMenu (\f a -> f { _doesUsePopUpMenu = a } )

-- | flag for embedding image as base64 in hdl file 
doesEmbedImage :: Simple Lens Settings Bool
doesEmbedImage = lens _doesEmbedImage (\f a -> f { _doesEmbedImage = a } )



-- | default hoodle state 
emptyHoodleState :: HoodleState 
emptyHoodleState = 
  HoodleState  
  { _hoodleModeState = ViewAppendState emptyGHoodle
  , _currFileName = Nothing 
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
  , _isOneTimeSelectMode = NoOneTimeSelectMode
  , _pageModeSignal = Nothing
  , _lastTimeCanvasConfigure = Nothing                      
  , _hookSet = Nothing
  , _tempQueue = emptyQueue
  , _tempLog = id 
  }

-- | default settings
defaultSettings :: Settings
defaultSettings = 
  Settings 
  { _doesUseXInput = False
  , _doesSmoothScroll = False
  , _doesUsePopUpMenu = True 
  , _doesEmbedImage = True 
  } 

-- | 
getHoodle :: HoodleState -> Hoodle EditMode 
getHoodle = either id gSelect2GHoodle . hoodleModeStateEither . view hoodleModeState 
--   where makehdl thdl = GHoodle (view gselTitle thdl) (view gembeddedpdf thdl) (view gselAll thdl)


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
      -- implement the following later
      -- page = gcast (unboxGet currentPage cinfobox) :: Page EditMode 
  in xstate { _currentCanvas = (cid,cinfobox)
            , _cvsInfoMap = cmap' }

-- | 

setCanvasId :: CanvasId -> CanvasInfoBox -> CanvasInfoBox 
setCanvasId cid = insideAction4CvsInfoBox (set canvasId cid) 
  
  
  -- (CanvasInfoBox cinfo) = CanvasInfoBox (cinfo { _canvasId = cid })


-- | 

modifyCanvasInfo :: CanvasId -> (CanvasInfoBox -> CanvasInfoBox) -> HoodleState
                    -> HoodleState
modifyCanvasInfo cid f xstate =  
    maybe xstate id . flip setCanvasInfoMap xstate 
                    . M.adjust f cid . getCanvasInfoMap 
    $ xstate 
  

{-
-- | should be deprecated
modifyCurrentCanvasInfo :: (CanvasInfoBox -> CanvasInfoBox) 
                        -> HoodleState
                        -> HoodleState
modifyCurrentCanvasInfo f = over currentCanvasInfo f  
-}

{-
-- | should be deprecated 
modifyCurrCvsInfoM :: (Monad m) => (CanvasInfoBox -> m CanvasInfoBox) 
                      -> HoodleState
                      -> m HoodleState
modifyCurrCvsInfoM f st = do 
  let cinfobox = view currentCanvasInfo st 
      cid = getCurrentCanvasId st 
  ncinfobox <- f cinfobox
  let cinfomap = getCanvasInfoMap st
      ncinfomap = M.adjust (const ncinfobox) cid cinfomap 
  maybe (return st) return (setCanvasInfoMap ncinfomap st) 
-}

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




