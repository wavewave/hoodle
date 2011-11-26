{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Application.HXournal.Type where

import Application.HXournal.Device
import Application.HXournal.Type.XournalBBox 
import Application.HXournal.Type.Event 

import Control.Monad.Coroutine 
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Functor.Identity (Identity(..))
import Control.Monad.State
import Data.Sequence

import Text.Xournal.Type
import Text.Xournal.Predefined 

import Graphics.UI.Gtk

import Data.Maybe

import Data.Label 
import Prelude hiding ((.), id)

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

type Trampoline m x = Coroutine Identity m x 
type Generator a m x = Coroutine (Yield a) m x
type Iteratee a m x = Coroutine (Await a) m x

type XournalStateIO = StateT HXournalState IO 

                  
data PenDraw = PenDraw { _points :: Seq (Double,Double) } 
             deriving (Show)


data PageMode = Continous | OnePage
              deriving (Show,Eq) 

data ZoomMode = Original | FitWidth | Zoom Double 
              deriving (Show,Eq)


data ViewInfo = ViewInfo { _pageMode :: PageMode
                         , _zoomMode :: ZoomMode
                         , _viewPortOrigin :: (Double,Double)
                         , _pageDimension :: (Double,Double) 
                         }
              deriving (Show)


data PenType = PenWork | HighlighterWork | EraserWork 
             deriving (Show,Eq)
                      
data PenColor = ColorBlack
              | ColorBlue 
              | ColorRed
              | ColorGreen
              | ColorGray
              | ColorLightBlue 
              | ColorLightGreen 
              | ColorMagenta
              | ColorOrange
              | ColorYellow
              | ColorWhite
              | ColorRGBA Double Double Double Double 
              deriving (Show,Eq,Ord)
      
data PenInfo = PenInfo { _penType :: PenType
                       , _penWidth :: Double
                       , _penColor :: PenColor } 
             deriving (Show) 

penColorNameMap :: M.Map PenColor B.ByteString                        
penColorNameMap = M.fromList [ (ColorBlack, "black")
                             , (ColorBlue , "blue")
                             , (ColorRed  , "red") 
                             , (ColorGreen, "green")
                             , (ColorGray,  "gray")
                             , (ColorLightBlue, "lightblue")
                             , (ColorLightGreen, "lightgreen")
                             , (ColorMagenta, "magenta")
                             , (ColorOrange, "orange")
                             , (ColorYellow, "yellow")
                             , (ColorWhite, "white") ]

penColorRGBAmap :: M.Map PenColor (Double,Double,Double,Double)
penColorRGBAmap = M.fromList $ map (\x->(fst x,fromJust (M.lookup (snd x) predefined_pencolor))) 
                             $ M.toList penColorNameMap 

convertPenColorToRGBA :: PenColor -> (Double,Double,Double,Double)
convertPenColorToRGBA (ColorRGBA r g b a) = (r,g,b,a)
convertPenColorToRGBA c = fromJust (M.lookup c penColorRGBAmap)

data HXournalState = HXournalState { _xournalbbox :: XournalBBox
                                   -- , _xournalbbox :: XournalBBox 
                                   , _drawArea :: DrawingArea
                                   , _currentPageNum :: Int
                                   , _currentPenDraw :: PenDraw
                                   , _callBack ::  MyEvent -> IO ()
                                   , _deviceList :: DeviceList
                                   , _viewInfo :: ViewInfo 
                                   , _penInfo :: PenInfo
                                   , _horizAdjustment :: Adjustment
                                   , _vertAdjustment :: Adjustment 
                                   } 


$(mkLabels [''PenDraw, ''ViewInfo, ''PenInfo, ''HXournalState]) 

emptyHXournalState :: HXournalState 
emptyHXournalState = 
  HXournalState  
  { _xournalbbox = mkXournalBBoxFromXournal emptyXournal
  -- , _xournalbbox = mkXournalBBoxFromXournal emptyXournal 
  , _drawArea = undefined
  , _currentPageNum = 0 
  , _currentPenDraw = PenDraw empty 
  , _callBack = undefined 
  , _deviceList = undefined
  , _viewInfo = ViewInfo OnePage Original (0,0) undefined 
  , _penInfo = PenInfo PenWork predefined_medium ColorBlack
  , _horizAdjustment = undefined             
  , _vertAdjustment = undefined 
  }

  