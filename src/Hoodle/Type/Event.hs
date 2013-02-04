-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Event 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Type.Event where

import Data.ByteString 
-- from other package
import Graphics.UI.Gtk
-- from hoodle-platform
-- import Data.Hoodle.BBox
import Data.Hoodle.Simple
-- from this package
import Hoodle.Device 
import Hoodle.Type.Clipboard
import Hoodle.Type.Enum
import Hoodle.Type.Canvas
import Hoodle.Type.PageArrangement

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
             | BackgroundStyleChanged BackgroundStyle 
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
             | ActionOrdered
             | GotOk
             | OkCancel Bool 
             | FileChosen (Maybe FilePath)
             | ColorChosen (Maybe PenColor) 
             | GotClipboardContent (Maybe [Item])
             | ContextMenuCreated
             | GotContextMenuSignal ContextMenuEvent
             | LaTeXInput (Maybe (ByteString,ByteString))
             | TextInput (Maybe String) 
             -- | EventConnected
             | EventDisconnected
             deriving (Show,Eq,Ord)


-- | 
data MenuEvent = MenuNew 
               | MenuAnnotatePDF
               | MenuOpen 
               | MenuSave
               | MenuSaveAs
               | MenuReload
               | MenuRecentDocument
               | MenuLoadPNGorJPG
               | MenuLoadSVG
               | MenuLaTeX
               | MenuEmbedPredefinedImage
               | MenuEmbedPredefinedImage2
               | MenuEmbedPredefinedImage3                 
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
               | MenuExportPageSVG
               | MenuNewLayer
               | MenuNextLayer
               | MenuPrevLayer 
               | MenuGotoLayer
               | MenuDeleteLayer
               | MenuPaperSize
               | MenuPaperColor
               | MenuPaperStyle 
               | MenuApplyToAllPages 
               | MenuEmbedAllPDFBkg
               --  | MenuLoadBackground
               --  | MenuBackgroundScreenshot 
               | MenuDefaultPaper
               | MenuSetAsDefaultPaper
               | MenuText 
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
               | MenuColorPicker
               | MenuDefaultPen 
               | MenuDefaultEraser 
               | MenuDefaultHighlighter
               | MenuDefaultText 
               | MenuSetAsDefaultOption
               | MenuRelaunch
               | MenuUseXInput
               | MenuSmoothScroll
               | MenuUsePopUpMenu
               | MenuEmbedImage
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

-- |
data ImgType = SVG | PDF 
               deriving (Show, Ord, Eq) 
                        
-- |                         
data ContextMenuEvent = CMenuSaveSelectionAs ImgType
                      | CMenuCut 
                      | CMenuCopy
                      | CMenuDelete
                      | CMenuCanvasView CanvasId PageNum Double Double  
                      | CMenuRotateCW
                      | CMenuRotateCCW 
                      | CMenuAutosavePage
                      | CMenuCustom
                      deriving (Show, Ord, Eq) 

-- | 
viewModeToMyEvent :: RadioAction -> IO MyEvent
viewModeToMyEvent a = do 
    v <- radioActionGetCurrentValue a
    case v of 
      1 -> return ToSinglePage
      0 -> return ToContSinglePage
      _ -> return ToSinglePage

