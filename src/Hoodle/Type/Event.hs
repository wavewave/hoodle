-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Event 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- GUI events in hoodle application
--
-----------------------------------------------------------------------------

module Hoodle.Type.Event where

-- from other package
import Data.ByteString 
import Data.IORef
import Data.Time.Clock
import Graphics.UI.Gtk
-- from hoodle-platform
import Data.Hoodle.Simple
-- from this package
import Hoodle.Device 
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
             | TouchDown Int PointerCoord
             | TouchMove Int PointerCoord
             | TouchUp Int PointerCoord 
             | PenColorChanged PenColor
             | PenWidthChanged Int 
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
             | AddLink (Maybe (String,FilePath))
             | EventDisconnected
             | GetHoodleFileInfo (IORef (Maybe String))
             | GotLink (Maybe String) (Int,Int)
             | Sync UTCTime 
             | FileReloadOrdered
             | CustomKeyEvent String 
             deriving Show
                      
instance Show (IORef a) where                      
  show _ = "IORef"

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
               | MenuStartSync
               | MenuQuit 
               | MenuUndo 
               | MenuRedo 
               | MenuCut 
               | MenuCopy 
               | MenuPaste 
               | MenuDelete
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
               | MenuDefaultPaper
               | MenuSetAsDefaultPaper
               | MenuText 
               | MenuAddLink
               | MenuShapeRecognizer
               | MenuRuler
               | MenuSelectRegion
               | MenuSelectRectangle
               | MenuVerticalSpace
               --   | MenuHandTool
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
               | MenuUseTouch 
               | MenuSmoothScroll
               | MenuUsePopUpMenu
               | MenuEmbedImage
               | MenuEmbedPDF
               | MenuTogglePanZoomWidget
               | MenuToggleLayerWidget
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
data ImgType = TypSVG | TypPDF 
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
                      | CMenuLinkConvert Link
                      | CMenuCreateALink 
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

