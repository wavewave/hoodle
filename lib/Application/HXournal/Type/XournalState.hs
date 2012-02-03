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

module Application.HXournal.Type.XournalState where

import Application.HXournal.Device
import Application.HXournal.Type.Event 
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Clipboard
import Application.HXournal.Type.Window 
import Application.HXournal.Type.Undo
import Application.HXournal.Type.Alias 
import Application.HXournal.Util
-- import Application.HXournal.NetworkClipboard.Client.Config
import Data.Xournal.Map
import Graphics.Xournal.Render.BBoxMapPDF
import Control.Category
import Control.Monad.State hiding (get,modify)
import Graphics.UI.Gtk hiding (Clipboard, get,set)
import Data.Maybe
import Data.Label 
import Data.Xournal.Generic
import qualified Data.IntMap as M

import Prelude hiding ((.), id)


type XournalStateIO = StateT HXournalState IO 

data XournalState = ViewAppendState { unView :: TXournalBBoxMapPDFBuf }
                  | SelectState { tempSelect :: TTempXournalSelectPDFBuf }
                    

data HXournalState = 
  HXournalState { _xournalstate :: XournalState
                , _currFileName :: Maybe FilePath
                , _canvasInfoMap :: CanvasInfoMap 
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
                --  , _networkClipboardInfo :: Maybe HXournalClipClientConfiguration
                } 


$(mkLabels [''HXournalState]) 

emptyHXournalState :: HXournalState 
emptyHXournalState = 
  HXournalState  
  { _xournalstate = ViewAppendState emptyGXournalMap
  , _currFileName = Nothing 
  , _canvasInfoMap = error "emptyHXournalState.canvasInfoMap"
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
--  , _networkClipboardInfo = Nothing 
  }

-- | 

getXournal :: HXournalState -> Xournal EditMode 
getXournal = either id makexoj . xojstateEither . get xournalstate 
  where makexoj txoj = GXournal (get g_selectTitle txoj) (get g_selectAll txoj)


-- | 
        
currentCanvasId :: HXournalState :-> CanvasId
currentCanvasId = lens getter setter 
  where getter = fst . _currentCanvas 
        setter a = modify currentCanvas (\(_,x)->(a,x))

currentCanvasInfo :: HXournalState :-> CanvasInfoBox
currentCanvasInfo = lens getter setter 
  where getter = snd . _currentCanvas
        setter a = modify currentCanvas (\(x,_)->(x,a)) 


resetXournalStateBuffers :: XournalState -> IO XournalState 
resetXournalStateBuffers xojstate1 = 
  case xojstate1 of 
    ViewAppendState xoj -> liftIO . liftM ViewAppendState . resetXournalBuffers $ xoj
    _ -> return xojstate1

getCanvasInfo :: CanvasId -> HXournalState -> CanvasInfoBox 
getCanvasInfo cid xstate = 
  let cinfoMap = get canvasInfoMap xstate
      maybeCvs = M.lookup cid cinfoMap
  in maybeError ("no canvas with id = " ++ show cid) maybeCvs


-- | change current canvas. this is the master function  

updateFromCanvasInfoAsCurrentCanvas :: CanvasInfoBox -> HXournalState -> HXournalState
updateFromCanvasInfoAsCurrentCanvas cinfobox xstate = 
  let cid = unboxGet canvasId cinfobox 
      cmap = get canvasInfoMap xstate
      cmap' = M.insert cid cinfobox cmap 
      -- implement the following later
      -- page = gcast (unboxGet currentPage cinfobox) :: Page EditMode 
  in xstate { _currentCanvas = (cid,cinfobox)
            , _canvasInfoMap = cmap' }

-- | 

setCanvasId :: CanvasId -> CanvasInfoBox -> CanvasInfoBox 
setCanvasId cid (CanvasInfoBox cinfo) = CanvasInfoBox (cinfo { _canvasId = cid })

-- | should be deprecated

modifyCurrentCanvasInfo :: (CanvasInfoBox -> CanvasInfoBox) 
                        -> HXournalState
                        -> HXournalState
modifyCurrentCanvasInfo f st =  modify currentCanvasInfo f . modify canvasInfoMap (M.adjust f cid) $ st 
  where cid = get currentCanvasId st 

-- | should be deprecated 
        
modifyCurrCvsInfoM :: (Monad m) => (CanvasInfoBox -> m CanvasInfoBox) 
                      -> HXournalState
                      -> m HXournalState
modifyCurrCvsInfoM f st = do 
  let cinfobox = get currentCanvasInfo st 
      cid = get currentCanvasId st 
  ncinfobox <- f cinfobox
  let cinfomap = get canvasInfoMap st
      ncinfomap = M.adjust (const ncinfobox) cid cinfomap 
      nst = set currentCanvasInfo ncinfobox 
            . set canvasInfoMap ncinfomap 
            $ st 
  return nst

-- | 

xojstateEither :: XournalState -> Either (Xournal EditMode) (Xournal SelectMode) 
xojstateEither xojstate = case xojstate of 
                            ViewAppendState xoj -> Left xoj 
                            SelectState txoj -> Right txoj 
                            


