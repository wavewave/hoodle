module Application.HXournal.Type.Event where

import Application.HXournal.Device 

data MyEvent = Initialized
             | CanvasConfigure Int Double Double 
             | UpdateCanvas Int
             | MenuNew 
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
             | MenuSetZoom
             | MenuFirstPage
             | MenuPreviousPage 
             | MenuNextPage 
             | MenuLastPage 
             | MenuShowLayer
             | MenuHideLayer
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
             | PenDown Int PointerCoord
             | PenMove Int PointerCoord
             | PenUp   Int PointerCoord 
             | HScrollBarMoved Int Double
             | VScrollBarMoved Int Double 
             deriving (Show,Eq,Ord)

