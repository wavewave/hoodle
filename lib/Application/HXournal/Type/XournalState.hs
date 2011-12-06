{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Application.HXournal.Type.XournalState where

import Application.HXournal.Device


import Application.HXournal.Type.Event 
import Application.HXournal.Type.Enum
import Application.HXournal.Type.Canvas

import Graphics.Xournal.Type
import Graphics.Xournal.Type.Select
import Graphics.Xournal.Type.Map
import Graphics.Xournal.Render.BBox 


import Control.Monad.State

import Text.Xournal.Type
import Text.Xournal.Predefined 

import Graphics.UI.Gtk

import Data.Maybe
import Data.Label 
import Prelude hiding ((.), id)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

type XournalStateIO = StateT HXournalState IO 

data XournalState = ViewAppendState { unView :: XournalBBoxMap }
                  -- | SelectState { unSelect :: XournalSelectMap }

data HXournalState = HXournalState { _xournalstate :: XournalState
                                   , _canvasInfoMap :: CanvasInfoMap 
                                   , _currentCanvas :: Int
                                   , _currentPenDraw :: PenDraw
                                   , _callBack ::  MyEvent -> IO ()
                                   , _deviceList :: DeviceList
                                   , _penInfo :: PenInfo
                                   , _selectInfo :: SelectInfo 
                                   } 


$(mkLabels [''HXournalState]) 

emptyHXournalState :: HXournalState 
emptyHXournalState = 
  HXournalState  
  { _xournalstate = ViewAppendState (mkXournalBBoxMapFromXournal emptyXournal)
  , _canvasInfoMap = error "emptyHXournalState.canvasInfoMap"
  , _currentCanvas = error "emtpyHxournalState.currentCanvas"
  , _currentPenDraw = emptyPenDraw 
  , _callBack = error "emtpyHxournalState.callBack"
  , _deviceList = error "emtpyHxournalState.deviceList"
  , _penInfo = PenInfo PenWork predefined_medium ColorBlack
  , _selectInfo = SelectInfo SelectRectangleWork 
  }

  