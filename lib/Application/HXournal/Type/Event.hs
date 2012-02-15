-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Type.Event 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.Type.Event where

import Application.HXournal.Type.Enum
import Application.HXournal.Device 
import Graphics.UI.Gtk

-- | 

data MyEvent = Initialized
             | CanvasConfigure Int Double Double 
             | UpdateCanvas Int
             | PenDown Int PenButton PointerCoord
             | PenMove Int PointerCoord
             | PenUp   Int PointerCoord 
             | PenColorChanged PenColor
             | PenWidthChanged Double 
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
             deriving (Show,Eq,Ord)

-- | 

data MenuEvent = MenuNew 
               | MenuAnnotatePDF
               | MenuOpen 
               | MenuSave
               | MenuSaveAs
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

