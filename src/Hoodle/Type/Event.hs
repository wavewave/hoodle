{-# LANGUAGE FlexibleInstances #-}

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
-- GUI events in hoodle application
--
-----------------------------------------------------------------------------

module Hoodle.Type.Event where

-- from other package
import           Control.Concurrent 
import           Data.ByteString 
import           Data.IORef
import qualified Data.Text as T
import           Data.Time.Clock
import           Graphics.UI.Gtk hiding (Image)
-- from hoodle-platform
import           Control.Monad.Trans.Crtn.Event 
import           Data.Hoodle.BBox
import           Data.Hoodle.Simple
-- from this package
import           Hoodle.Device 
import           Hoodle.Type.Enum
import           Hoodle.Type.Canvas
import           Hoodle.Type.PageArrangement

-- | 
data AllEvent = UsrEv UserEvent | SysEv SystemEvent
              deriving Show 

-- | 
data SystemEvent = TestSystemEvent | ClockUpdateEvent
                 deriving Show 
                          
-- | 
data UserEvent = Initialized
               | CanvasConfigure Int Double Double 
               | UpdateCanvas Int
               | UpdateCanvasEfficient Int
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
               | GotRevision String String
               | GotRevisionInk String [Stroke]
               | ChangeDialog
               | ActionOrdered                 
               | MiniBuffer MiniBufferEvent
               | MultiLine MultiLineEvent
               | NetworkProcess NetworkEvent
               | ImageFileDropped FilePath
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
               | MenuCombineLaTeX
               | MenuEmbedPredefinedImage
               | MenuEmbedPredefinedImage2
               | MenuEmbedPredefinedImage3                 
               | MenuPrint 
               | MenuExport 
               | MenuStartSync
               | MenuVersionSave
               | MenuShowRevisions
               | MenuShowUUID
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
               | MenuFollowLinks
               | MenuKeepAspectRatio
               | MenuTogglePanZoomWidget
               | MenuToggleLayerWidget
               | MenuToggleClockWidget
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
               deriving Show -- (Show, Ord, Eq)

-- |
data ImgType = TypSVG | TypPDF 
               deriving (Show, Ord, Eq) 
                        
-- |                         
data ContextMenuEvent = CMenuSaveSelectionAs ImgType
                      | CMenuCut 
                      | CMenuCopy
                      | CMenuDelete
                      | CMenuCanvasView CanvasId PageNum Double Double  
                      | CMenuAutosavePage
                      | CMenuLinkConvert Link
                      | CMenuCreateALink 
                      | CMenuAssocWithNewFile
                      | CMenuPangoConvert (Double,Double) T.Text
                      | CMenuLaTeXConvert (Double,Double) T.Text
                      | CMenuLaTeXConvertNetwork (Double,Double) T.Text
                      | CMenuCropImage (BBoxed Image)
                      | CMenuRotate    RotateDir (BBoxed Image)
                      | CMenuCustom
                      deriving (Show, Ord, Eq) 


-- | event for minibuffer operation (currently pen only) 
data MiniBufferEvent = MiniBufferInitialized DrawWindow
                     | MiniBufferPenDown PenButton PointerCoord
                     | MiniBufferPenUp PointerCoord
                     | MiniBufferPenMove PointerCoord
                     deriving Show

instance Show DrawWindow where
  show _ = "DrawWindow"

-- | event for multiline text view/buffer
data MultiLineEvent = MultiLineChanged T.Text
                    deriving Show

-- | event for network
data NetworkEvent = NetworkDialog 
                  | NetworkInitialized ThreadId (MVar ())
                  | NetworkReceived T.Text
                  | NetworkCloseDialog
                  | NetworkClosed
                  deriving Show

instance Show (MVar ()) where
  show _ = "MVar"

-- | 
viewModeToUserEvent :: RadioAction -> IO UserEvent
viewModeToUserEvent a = do 
    v <- radioActionGetCurrentValue a
    case v of 
      1 -> return ToSinglePage
      0 -> return ToContSinglePage
      _ -> return ToSinglePage

-- | 
mkIOaction :: ((AllEvent -> IO ()) -> IO AllEvent) -> Either (ActionOrder AllEvent) AllEvent
mkIOaction = Left . ActionOrder  



