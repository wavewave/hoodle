{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hoodle.Type.Event where

import Control.Concurrent
import Control.Monad.Trans.Crtn.Event
import Data.ByteString
import Data.Hoodle.BBox
import Data.Hoodle.Simple
import Data.IORef
import qualified Data.Text as T
import Data.UUID (UUID)
import Graphics.Hoodle.Render.Type
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk as Gtk
import Hoodle.Device
import Hoodle.Type.Enum
import Hoodle.Type.PageArrangement
import Hoodle.Util

-- |
data AllEvent = UsrEv UserEvent | SysEv SystemEvent
  deriving (Show)

-- | orphan instance for Surface for convenience
instance Show Cairo.Surface where
  show _ = "cairo surface"

-- |
data SystemEvent
  = TestSystemEvent
  | ClockUpdateEvent
  | RenderCacheUpdate (SurfaceID, (Double, Cairo.Surface))
  deriving (Show)

-- |
data UserEvent
  = Initialized (Maybe FilePath)
  | CanvasConfigure Int Double Double
  | UpdateCanvas Int
  | UpdateCanvasEfficient Int
  | PenDown Int PenButton PointerCoord
  | PenMove Int PointerCoord
  | PenUp Int PointerCoord
  | TouchDown Int PointerCoord
  | TouchMove Int PointerCoord
  | TouchUp Int PointerCoord
  | PenColorChanged PenColor
  | PenWidthChanged Int
  | AssignPenMode (Either PenType SelectType)
  | BackgroundStyleChanged BackgroundStyle
  | AssignNewPageMode NewPageModeType
  | HScrollBarMoved Int Double
  | VScrollBarMoved Int Double
  | VScrollBarStart Int Double
  | VScrollBarEnd Int Double
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
  | LaTeXInput (Maybe (ByteString, ByteString))
  | TextInput (Maybe String)
  | AddLink (Maybe (String, FilePath))
  | OpenLink UrlPath (Maybe (T.Text, T.Text))
  | OpenShared UUID
  | OpenTemp UUID FilePath
  | EventDisconnected
  | GetHoodleFileInfo (IORef (Maybe String))
  | GetHoodleFileInfoFromTab UUID (IORef (Maybe String))
  | GotLink (Maybe String) (Int, Int)
  | FileReloadOrdered
  | CustomKeyEvent String
  | GotRevision String String
  | GotRevisionInk String [Stroke]
  | ChangeDialog
  | ActionOrdered
  | GotRecogResult Bool T.Text
  | MiniBuffer MiniBufferEvent
  | MultiLine MultiLineEvent
  | RenderEv RenderEvent
  | LinePosition (Maybe (Int, Int))
  | Keyword (Maybe T.Text)
  | SwitchTab Int
  | CloseTab UUID
  | UIEv UIEvent
  | NetworkProcess NetworkEvent
  deriving (Show)

-- | orphan instance for IORef for convenience
instance Show (IORef a) where
  show _ = "IORef"

newtype UIEvent = UIGetFlag Bool
  deriving (Show)

data RenderEvent
  = GotRItem RItem
  | GotRItems [RItem]
  | GotRBackground RBackground
  | GotRHoodle RHoodle
  | GotNone
  | FinishCommand SurfaceID
  deriving (Show)

-- |
data MenuEvent
  = MenuNew
  | MenuAnnotatePDF
  | MenuOpen
  | MenuSave
  | MenuSaveAs
  | MenuReload
  | MenuRecentDocument
  | MenuLoadPNGorJPG
  | MenuLoadSVG
  | MenuLoadImageBackground
  | MenuText
  | MenuEmbedTextSource
  | MenuEditEmbedTextSource
  | MenuTextFromSource
  | MenuLaTeX
  | MenuCombineLaTeX
  | MenuLaTeXFromSource
  | MenuUpdateLaTeX
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
  | MenuAddLink
  | MenuAddAnchor
  | MenuListAnchors
  | MenuHandwritingRecognitionDialog
  | MenuSelectRegion
  | MenuSelectRectangle
  | MenuVerticalSpace
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
  | MenuUsePopUpMenu
  | MenuEmbedImage
  | MenuEmbedPDF
  | MenuFollowLinks
  | MenuKeepAspectRatio
  | MenuUseVariableCursor
  | MenuTogglePanZoomWidget
  | MenuToggleLayerWidget
  | MenuToggleClockWidget
  | MenuToggleScrollWidget
  | MenuPressureSensitivity
  | MenuAddTab
  | MenuCloseTab
  | MenuAbout
  | MenuDefault
  | MenuEditNetEmbedTextSource
  | MenuToggleNetworkEditSource
  | MenuLaTeXNetwork
  deriving (Show)

-- |
data ImgType = TypSVG | TypPDF
  deriving (Show, Ord, Eq)

-- |
data ContextMenuEvent
  = CMenuSaveSelectionAs ImgType
  | CMenuCut
  | CMenuCopy
  | CMenuDelete
  | CMenuCanvasView CanvasId PageNum Double Double
  | CMenuAutosavePage
  | CMenuLinkConvert Link
  | CMenuCreateALink
  | CMenuAssocWithNewFile
  | CMenuMakeLinkToAnchor Anchor
  | CMenuPangoConvert (Double, Double) T.Text
  | CMenuLaTeXConvert (Double, Double) T.Text
  | CMenuLaTeXConvertNetwork (Double, Double) T.Text
  | CMenuLaTeXUpdate (Double, Double) Dimension T.Text
  | CMenuCropImage (BBoxed Image)
  | CMenuRotate RotateDir (BBoxed Image)
  | CMenuExport (BBoxed Image)
  | CMenuExportHoodlet Item
  | CMenuConvertSelection Item
  | CMenuCustom
  deriving (Show, Ord, Eq)

-- | event for minibuffer operation (currently pen only)
data MiniBufferEvent
  = MiniBufferInitialized Gtk.DrawWindow
  | MiniBufferPenDown PenButton PointerCoord
  | MiniBufferPenUp PointerCoord
  | MiniBufferPenMove PointerCoord
  deriving (Show)

-- | orphan instance for DrawWindow for convenience
instance Show Gtk.DrawWindow where
  show _ = "DrawWindow"

-- | event for multiline text view/buffer
newtype MultiLineEvent = MultiLineChanged T.Text
  deriving (Show)

-- | event for network
data NetworkEvent
  = NetworkDialog
  | NetworkInitialized ThreadId (MVar ())
  | NetworkReceived T.Text
  | NetworkCloseDialog
  | NetworkClosed
  deriving (Show)

-- | orphan instance for MVar for convenience
instance Show (MVar ()) where
  show _ = "MVar"

-- |
viewModeToUserEvent :: Gtk.RadioAction -> IO UserEvent
viewModeToUserEvent a = do
  v <- Gtk.radioActionGetCurrentValue a
  case v of
    1 -> return ToSinglePage
    0 -> return ToContSinglePage
    _ -> return ToSinglePage

-- |
mkIOaction :: ((AllEvent -> IO ()) -> IO AllEvent) -> Either (ActionOrder AllEvent) AllEvent
mkIOaction = Left . ActionOrder
