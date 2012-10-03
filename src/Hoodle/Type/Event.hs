-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Event 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Type.Event where

-- from other package
import Graphics.UI.Gtk
-- from hoodle-platform
-- import Data.Hoodle.BBox
import Data.Hoodle.Simple
-- from this package
import Hoodle.Device 
import Hoodle.Type.Clipboard
import Hoodle.Type.Enum

-- | 

data MyEvent = Initialized
             | CanvasConfigure Int Double Double 
             | UpdateCanvas Int
             | PenDown Int PenButton PointerCoord
             | PenMove Int PointerCoord
             | PenUp   Int PointerCoord 
             | PenColorChanged PenColor
             | PenWidthChanged Int -- (PenType -> Double)
             | AssignPenMode (Either PenType SelectType) 
             | HScrollBarMoved Int Double
             | VScrollBarMoved Int Double 
             | VScrollBarStart Int Double
             | VScrollBarEnd   Int Double
             | PaneMoveStart 
             | PaneMoveEnd 
             | ToViewAppendMode
             | ToSelectMode
             | ToSinglePage
             | ToContSinglePage
             | Menu MenuEvent 
             | GotClipboardContent (Maybe [Item])
             | ActionOrdered
             | OkCancel Bool 
             | FileChosen (Maybe FilePath)
             deriving (Show,Eq,Ord)


-- | 
data MenuEvent = MenuNew 
               | MenuAnnotatePDF
               | MenuLoadImage
               | MenuOpen 
               | MenuSave
               | MenuSaveAs
               | MenuReload
               | MenuRecentDocument
               | MenuPrint 
               | MenuExport 
               | MenuQuit 
               | MenuUndo 
               | MenuRedo 
               | MenuCut 
               | MenuCopy 
               | MenuPaste 
               | MenuDelete
               --    | MenuNetCopy
               --    | MenuNetPaste
               | MenuFullScreen 
               | MenuZoom 
               | MenuZoomIn
               | MenuZoomOut 
               | MenuNormalSize
               | MenuPageWidth
               | MenuPageHeight
               | MenuSetZoom
               | MenuFirstPage
               | MenuPreviousPage 
               | MenuNextPage 
               | MenuLastPage 
               | MenuShowLayer
               | MenuHideLayer
               | MenuHSplit  
               | MenuVSplit
               | MenuDelCanvas
               | MenuNewPageBefore
               | MenuNewPageAfter 
               | MenuNewPageAtEnd 
               | MenuDeletePage
               | MenuNewLayer
               | MenuNextLayer
               | MenuPrevLayer 
               | MenuGotoLayer
               | MenuDeleteLayer
               | MenuPaperSize
               | MenuPaperColor
               | MenuPaperStyle 
               | MenuApplyToAllPages 
               | MenuLoadBackground
               | MenuBackgroundScreenshot 
               | MenuDefaultPaper
               | MenuSetAsDefaultPaper
               | MenuShapeRecognizer
               | MenuRuler
               | MenuSelectRegion
               | MenuSelectRectangle
               | MenuVerticalSpace
               | MenuHandTool
               | MenuPenOptions
               | MenuEraserOptions 
               | MenuHighlighterOptions
               | MenuTextFont
               | MenuDefaultPen 
               | MenuDefaultEraser 
               | MenuDefaultHighlighter
               | MenuDefaultText 
               | MenuSetAsDefaultOption
               | MenuRelaunch
               | MenuUseXInput
               | MenuDiscardCoreEvents 
               | MenuEraserTip 
               | MenuPressureSensitivity
               | MenuPageHighlight
               | MenuMultiplePageView
               | MenuMultiplePages
               | MenuButton2Mapping
               | MenuButton3Mapping 
               | MenuAntialiasedBitmaps
               | MenuProgressiveBackgrounds
               | MenuPrintPaperRuling 
               | MenuLeftHandedScrollbar
               | MenuShortenMenus
               | MenuAutoSavePreferences
               | MenuSavePreferences
               | MenuAbout
               | MenuDefault
               deriving (Show, Ord, Eq)
  
viewModeToMyEvent :: RadioAction -> IO MyEvent
viewModeToMyEvent a = do 
    v <- radioActionGetCurrentValue a
    case v of 
      1 -> return ToSinglePage
      0 -> return ToContSinglePage
      _ -> return ToSinglePage

