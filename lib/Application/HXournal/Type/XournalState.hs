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

module Application.HXournal.Type.XournalState where

import Application.HXournal.Device
import Application.HXournal.Type.Event 

import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Clipboard
import Application.HXournal.Type.Window 
import Application.HXournal.Type.Undo

-- import Application.HXournal.NetworkClipboard.Client.Config
import Data.Xournal.Map

import Graphics.Xournal.Render.BBoxMapPDF

import Control.Applicative
import Control.Category
import Control.Monad.State hiding (get,modify)


import Graphics.UI.Gtk hiding (Clipboard, get,set)
import Data.Maybe
import Data.Label 
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

modifyCurrentCanvasInfo :: (CanvasInfoBox -> CanvasInfoBox) 
                        -> HXournalState
                        -> HXournalState
modifyCurrentCanvasInfo f st =  modify currentCanvasInfo f . modify canvasInfoMap (M.adjust f cid) $ st 
  where cid = fst . get currentCanvas $ st 











