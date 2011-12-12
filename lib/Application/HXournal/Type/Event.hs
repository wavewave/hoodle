module Application.HXournal.Type.Event where

import Application.HXournal.Type.Enum
import Application.HXournal.Device 

data MyEvent = Initialized
             | CanvasConfigure Int Double Double 
             | UpdateCanvas Int
             | PenDown Int PointerCoord
             | PenMove Int PointerCoord
             | PenUp   Int PointerCoord 
             | PenColorChanged PenColor
             | PenWidthChanged Double 
             | HScrollBarMoved Int Double
             | VScrollBarMoved Int Double 
             | VScrollBarStart Int Double
             | VScrollBarEnd   Int Double
             | ToViewAppendMode
             | ToSelectMode
             | Menu MenuEvent 
             deriving (Show,Eq,Ord)


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
  