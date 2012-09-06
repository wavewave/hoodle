{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.XournalState 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Type.XournalState 
( -- XournalStateIO 
XournalState(..)      
, HoodleState(..)
, IsOneTimeSelectMode(..)
-- | labels
, xournalstate
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
, isOneTimeSelectMode
, pageModeSignal
, lastTimeCanvasConfigure
, hookSet 
, tempLog 
, tempQueue 
-- | others 
, emptyHoodleState
, getXournal
-- | additional lenses 
, getCanvasInfoMap 
, setCanvasInfoMap 
, getCurrentCanvasId
, setCurrentCanvasId
, currentCanvasInfo
, resetXournalStateBuffers
, getCanvasInfo
, setCanvasInfo
, updateFromCanvasInfoAsCurrentCanvas
, setCanvasId
, modifyCanvasInfo
, modifyCurrentCanvasInfo
, modifyCurrCvsInfoM
, xojstateEither
, getCurrentPageFromXojState
, getCurrentPageDimFromXojState
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
import           Data.Xournal.Generic
import           Data.Xournal.Map
import           Graphics.Xournal.Render.BBoxMapPDF
-- from this package 
import           Hoodle.Device
import           Hoodle.Script.Hook
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


-- type XournalStateIO = StateT HoodleState IO 

-- | 

data XournalState = ViewAppendState { unView :: TXournalBBoxMapPDFBuf }
                  | SelectState { tempSelect :: TTempXournalSelectPDFBuf }
                    
-- | 

data IsOneTimeSelectMode = NoOneTimeSelectMode 
                         | YesBeforeSelect 
                         | YesAfterSelect
                         deriving (Show,Eq,Ord)

data HoodleState = 
  HoodleState { _xournalstate :: XournalState
                , _currFileName :: Maybe FilePath
                , _cvsInfoMap :: CanvasInfoMap 
                , _currentCanvas :: (CanvasId,CanvasInfoBox)
                , _frameState :: WindowConfig 
                , _rootWindow :: Widget
                , _rootContainer :: Box
                , _rootOfRootWindow :: Window
                , _currentPenDraw :: PenDraw
    --            , _clipboard :: Clipboard
                , _callBack ::  MyEvent -> IO ()
                , _deviceList :: DeviceList
                , _penInfo :: PenInfo
                , _selectInfo :: SelectInfo 
                , _gtkUIManager :: UIManager 
                , _isSaved :: Bool 
                , _undoTable :: UndoTable XournalState
                , _isOneTimeSelectMode :: IsOneTimeSelectMode
                , _pageModeSignal :: Maybe (ConnectId RadioAction)
                , _lastTimeCanvasConfigure :: Maybe UTCTime 
                , _hookSet :: Maybe Hook
                , _tempQueue :: Queue (Either (ActionOrder MyEvent) MyEvent)
                , _tempLog :: String -> String 
                } 


makeLenses ''HoodleState 

emptyHoodleState :: HoodleState 
emptyHoodleState = 
  HoodleState  
  { _xournalstate = ViewAppendState emptyGXournalMap
  , _currFileName = Nothing 
  , _cvsInfoMap = error "emptyHoodleState.cvsInfoMap"
  , _currentCanvas = error "emtpyHxournalState.currentCanvas"
  , _frameState = error "emptyHoodleState.frameState" 
  , _rootWindow = error "emtpyHoodleState.rootWindow"
  , _rootContainer = error "emptyHoodleState.rootContainer"
  , _rootOfRootWindow = error "emptyHoodleState.rootOfRootWindow"
  , _currentPenDraw = emptyPenDraw 
  -- , _clipboard = emptyClipboard
  , _callBack = error "emtpyHxournalState.callBack"
  , _deviceList = error "emtpyHxournalState.deviceList"
  , _penInfo = defaultPenInfo 
  , _selectInfo = SelectInfo SelectRectangleWork 
  , _gtkUIManager = error "emptyHoodleState.gtkUIManager"
  , _isSaved = False 
  , _undoTable = emptyUndo 1 
  -- , _isEventBlocked = False 
  , _isOneTimeSelectMode = NoOneTimeSelectMode
  , _pageModeSignal = Nothing
  , _lastTimeCanvasConfigure = Nothing                      
  , _hookSet = Nothing
  , _tempQueue = emptyQueue
  , _tempLog = id 
  }

-- | 

getXournal :: HoodleState -> Xournal EditMode 
getXournal = either id makexoj . xojstateEither . view xournalstate 
  where makexoj txoj = GXournal (view g_selectTitle txoj) (view g_selectAll txoj)


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

resetXournalStateBuffers :: XournalState -> IO XournalState 
resetXournalStateBuffers xojstate1 = 
  case xojstate1 of 
    ViewAppendState xoj -> liftIO . liftM ViewAppendState . resetXournalBuffers $ xoj
    _ -> return xojstate1

-- |
    
getCanvasInfo :: CanvasId -> HoodleState -> CanvasInfoBox 
getCanvasInfo cid xstate = 
  let cinfoMap = getCanvasInfoMap xstate
      maybeCvs = M.lookup cid cinfoMap
  in maybeError ("no canvas with id = " ++ show cid) maybeCvs

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
setCanvasId cid (CanvasInfoBox cinfo) = CanvasInfoBox (cinfo { _canvasId = cid })


-- | 

modifyCanvasInfo :: CanvasId -> (CanvasInfoBox -> CanvasInfoBox) -> HoodleState
                    -> HoodleState
modifyCanvasInfo cid f xstate =  
    maybe xstate id . flip setCanvasInfoMap xstate 
                    . M.adjust f cid . getCanvasInfoMap 
    $ xstate 
  

-- | should be deprecated

modifyCurrentCanvasInfo :: (CanvasInfoBox -> CanvasInfoBox) 
                        -> HoodleState
                        -> HoodleState
modifyCurrentCanvasInfo f = over currentCanvasInfo f  
  

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


-- | 

xojstateEither :: XournalState -> Either (Xournal EditMode) (Xournal SelectMode) 
xojstateEither xojstate = case xojstate of 
                            ViewAppendState xoj -> Left xoj 
                            SelectState txoj -> Right txoj 

-- | 
 
getCurrentPageFromXojState :: (ViewMode a) => CanvasInfo a 
                              -> XournalState -> Page EditMode 
getCurrentPageFromXojState cinfo xojstate = 
  let cpn = view currentPageNum cinfo 
      pagemap = getPageMapFromXojState xojstate   
  in maybeError "updatePageFromCanvasToXournal" $ M.lookup cpn pagemap 

-- | 

getCurrentPageDimFromXojState :: (ViewMode a) => CanvasInfo a 
                              -> XournalState -> PageDimension
getCurrentPageDimFromXojState cinfo =                               
  PageDimension . view g_dimension . getCurrentPageFromXojState cinfo


-- | 

getPageMapFromXojState :: XournalState -> M.IntMap (Page EditMode)
getPageMapFromXojState = either (view g_pages) (view g_selectAll) . xojstateEither 
  
      
-- | 

showCanvasInfoMapViewPortBBox :: HoodleState -> IO ()
showCanvasInfoMapViewPortBBox xstate = do 
  let cmap = getCanvasInfoMap xstate
  print . map (unboxGet (viewInfo.pageArrangement.viewPortBBox)) . M.elems $ cmap 




