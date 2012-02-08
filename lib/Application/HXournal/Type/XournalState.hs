{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Type.XournalState 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.Type.XournalState 
( XournalStateIO 
, XournalState(..)      
, HXournalState(..)
-- | labels
, xournalstate
, currFileName
-- , canvasInfoMap 
-- , currentCanvas
, frameState
, rootWindow
, rootContainer
, rootOfRootWindow
, currentPenDraw
, clipboard
, callBack
, deviceList
, penInfo
, selectInfo
, gtkUIManager
, isSaved
, undoTable
, isEventBlocked 
, pageModeSignal
-- | others 
, emptyHXournalState
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
-- | for debug
, showCanvasInfoMapViewPortBBox
) where

import Application.HXournal.Device
import Application.HXournal.Type.Event 
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Clipboard
import Application.HXournal.Type.Window 
import Application.HXournal.Type.Undo
import Application.HXournal.Type.Alias 
import Application.HXournal.Type.PageArrangement
import Application.HXournal.Util
-- import Application.HXournal.NetworkClipboard.Client.Config
import Data.Xournal.Map
import Graphics.Xournal.Render.BBoxMapPDF
import Control.Category
import Control.Monad.State hiding (get,modify)
import Graphics.UI.Gtk hiding (Clipboard, get,set)
import Data.Maybe
import Data.Label 
import qualified Data.Label.Maybe as ML
import Data.Xournal.Generic
import qualified Data.IntMap as M

import Prelude hiding ((.), id)


type XournalStateIO = StateT HXournalState IO 

data XournalState = ViewAppendState { unView :: TXournalBBoxMapPDFBuf }
                  | SelectState { tempSelect :: TTempXournalSelectPDFBuf }
                    

data HXournalState = 
  HXournalState { _xournalstate :: XournalState
                , _currFileName :: Maybe FilePath
                , _cvsInfoMap :: CanvasInfoMap 
                , _currentCanvas :: (CanvasId,CanvasInfoBox)
                , _frameState :: WindowConfig 
                , _rootWindow :: Widget
                , _rootContainer :: Box
                , _rootOfRootWindow :: Window
                , _currentPenDraw :: PenDraw
                , _clipboard :: Clipboard
                , _callBack ::  MyEvent -> IO ()
                , _deviceList :: DeviceList
                , _penInfo :: PenInfo
                , _selectInfo :: SelectInfo 
                , _gtkUIManager :: UIManager 
                , _isSaved :: Bool 
                , _undoTable :: UndoTable XournalState
                , _isEventBlocked :: Bool
                , _pageModeSignal :: Maybe (ConnectId RadioAction)
                --  , _networkClipboardInfo :: Maybe HXournalClipClientConfiguration
                } 


$(mkLabels [''HXournalState]) 

emptyHXournalState :: HXournalState 
emptyHXournalState = 
  HXournalState  
  { _xournalstate = ViewAppendState emptyGXournalMap
  , _currFileName = Nothing 
  , _cvsInfoMap = error "emptyHXournalState.cvsInfoMap"
  , _currentCanvas = error "emtpyHxournalState.currentCanvas"
  , _frameState = error "emptyHXournalState.frameState" 
  , _rootWindow = error "emtpyHXournalState.rootWindow"
  , _rootContainer = error "emptyHXournalState.rootContainer"
  , _rootOfRootWindow = error "emptyHXournalState.rootOfRootWindow"
  , _currentPenDraw = emptyPenDraw 
  , _clipboard = emptyClipboard
  , _callBack = error "emtpyHxournalState.callBack"
  , _deviceList = error "emtpyHxournalState.deviceList"
  , _penInfo = defaultPenInfo 
  , _selectInfo = SelectInfo SelectRectangleWork 
  , _gtkUIManager = error "emptyHXournalState.gtkUIManager"
  , _isSaved = False 
  , _undoTable = emptyUndo 1 
  , _isEventBlocked = False 
  , _pageModeSignal = Nothing
--  , _networkClipboardInfo = Nothing 
  }

-- | 

getXournal :: HXournalState -> Xournal EditMode 
getXournal = either id makexoj . xojstateEither . get xournalstate 
  where makexoj txoj = GXournal (get g_selectTitle txoj) (get g_selectAll txoj)


-- | 
        
getCurrentCanvasId :: HXournalState -> CanvasId
getCurrentCanvasId = fst . _currentCanvas 
  
-- | 

setCurrentCanvasId :: CanvasId -> HXournalState -> Maybe HXournalState
setCurrentCanvasId a f = do 
    cinfobox <- M.lookup a (_cvsInfoMap f)
    return (f { _currentCanvas = (a,cinfobox) })
     
-- | 
    
getCanvasInfoMap :: HXournalState -> CanvasInfoMap 
getCanvasInfoMap = _cvsInfoMap 

-- | 

setCanvasInfoMap :: CanvasInfoMap -> HXournalState -> Maybe HXournalState 
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

currentCanvasInfo :: HXournalState :-> CanvasInfoBox
currentCanvasInfo = lens getter setter 
  where 
    getter = snd . _currentCanvas 
    setter a f =  
      let cid = fst . _currentCanvas $ f 
          cmap' = M.adjust (const a) cid (_cvsInfoMap f)
      in f { _currentCanvas = (cid,a), _cvsInfoMap = cmap' }

resetXournalStateBuffers :: XournalState -> IO XournalState 
resetXournalStateBuffers xojstate1 = 
  case xojstate1 of 
    ViewAppendState xoj -> liftIO . liftM ViewAppendState . resetXournalBuffers $ xoj
    _ -> return xojstate1

-- |
    
getCanvasInfo :: CanvasId -> HXournalState -> CanvasInfoBox 
getCanvasInfo cid xstate = 
  let cinfoMap = getCanvasInfoMap xstate
      maybeCvs = M.lookup cid cinfoMap
  in maybeError ("no canvas with id = " ++ show cid) maybeCvs

-- | 

setCanvasInfo :: (CanvasId,CanvasInfoBox) -> HXournalState -> HXournalState 
setCanvasInfo (cid,cinfobox) xstate = 
  let cmap = getCanvasInfoMap xstate
      cmap' = M.insert cid cinfobox cmap 
  in maybe xstate id $ setCanvasInfoMap cmap' xstate



-- | change current canvas. this is the master function  

updateFromCanvasInfoAsCurrentCanvas :: CanvasInfoBox -> HXournalState -> HXournalState
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

modifyCanvasInfo :: CanvasId -> (CanvasInfoBox -> CanvasInfoBox) -> HXournalState
                    -> HXournalState
modifyCanvasInfo cid f xstate =  
    maybe xstate id . flip setCanvasInfoMap xstate 
                    . M.adjust f cid . getCanvasInfoMap 
    $ xstate 
  

-- | should be deprecated

modifyCurrentCanvasInfo :: (CanvasInfoBox -> CanvasInfoBox) 
                        -> HXournalState
                        -> HXournalState
modifyCurrentCanvasInfo f = modify currentCanvasInfo f  
  

-- | should be deprecated 
        
modifyCurrCvsInfoM :: (Monad m) => (CanvasInfoBox -> m CanvasInfoBox) 
                      -> HXournalState
                      -> m HXournalState
modifyCurrCvsInfoM f st = do 
  let cinfobox = get currentCanvasInfo st 
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

showCanvasInfoMapViewPortBBox :: HXournalState -> IO ()
showCanvasInfoMapViewPortBBox xstate = do 
  let cmap = getCanvasInfoMap xstate
  putStrLn . show . map (unboxGet (viewPortBBox.pageArrangement.viewInfo)) . M.elems $ cmap 


