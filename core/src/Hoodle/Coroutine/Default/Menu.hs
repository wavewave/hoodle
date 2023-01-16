{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-incomplete-record-updates #-}

module Hoodle.Coroutine.Default.Menu where

import Control.Lens (set, view, (.~), _1)
import Control.Monad.IO.Class ()
import Control.Monad.State (get, gets, liftIO, modify, void)
import Data.Foldable (mapM_)
import Data.Hoodle.Generic (gbackground, gpages)
import Data.Hoodle.Select (gselAll)
import qualified Data.IntMap as M
import Data.Monoid ()
import Graphics.Hoodle.Render.Type
  ( RBackground (..),
    isRBkgSmpl,
  )
import qualified Graphics.UI.Gtk as Gtk
import Hoodle.Accessor (pureUpdateUhdl)
import Hoodle.Coroutine.Commit (redo, undo)
import Hoodle.Coroutine.Draw
  ( invalidateAll,
    invalidateInBBox,
    nextevent,
    updateFlagFromToggleUI,
  )
import Hoodle.Coroutine.File
  ( askIfSave,
    askQuitProgram,
    embedAllPDFBackground,
    embedHoodlet,
    embedPredefinedImage,
    embedPredefinedImage2,
    embedPredefinedImage3,
    exportCurrentPageAsSVG,
    fileAnnotatePDF,
    fileExport,
    fileLoadImageBackground,
    fileLoadPNGorJPG,
    fileLoadSVG,
    fileOpen,
    fileReload,
    fileSave,
    fileSaveAs,
    fileShowRevisions,
    fileShowUUID,
    fileVersionSave,
  )
import Hoodle.Coroutine.LaTeX (laTeXFooter, laTeXHeader)
import Hoodle.Coroutine.Layer
  ( deleteCurrentLayer,
    gotoNextLayer,
    gotoPrevLayer,
    makeNewLayer,
    startGotoLayerAt,
  )
import Hoodle.Coroutine.Link (addLink, listAnchors)
import Hoodle.Coroutine.Mode (modeChange)
import Hoodle.Coroutine.Page
  ( changePage,
    deleteCurrentPage,
    newPage,
    pageZoomChange,
    pageZoomChangeRel,
  )
import Hoodle.Coroutine.Select.Clipboard
  ( copySelection,
    cutSelection,
    deleteSelection,
    pasteToSelection,
  )
import Hoodle.Coroutine.TextInput
  ( addAnchor,
    combineLaTeXText,
    editEmbeddedTextSource,
    editNetEmbeddedTextSource,
    embedTextSource,
    laTeXInput,
    laTeXInputFromSource,
    laTeXInputNetwork,
    textInput,
    textInputFromSource,
    toggleNetworkEditSource,
  )
import Hoodle.Coroutine.Window
  ( addTab,
    closeTab,
    deleteCanvas,
    eitherSplit,
    fullScreen,
  )
import Hoodle.GUI.Reflect (reflectCursor)
import Hoodle.Script (relaunchApplication)
import Hoodle.Type.Canvas
  ( currentTool,
    penColor,
    variableWidthPen,
  )
import Hoodle.Type.Coroutine (MainCoroutine, doIOaction)
import Hoodle.Type.Enum
  ( AddDirection (PageAfter, PageBefore),
    DrawFlag (Efficient),
    PenColor (ColorRGBA),
    ZoomModeRel (..),
    convertBackgroundStyleToByteString,
    convertPenColorToRGBA,
  )
import Hoodle.Type.Event (AllEvent (..), MenuEvent (..), UserEvent (..))
import Hoodle.Type.HoodleState
  ( FileStore (LocalDir),
    HoodleModeState (ViewAppendState),
    backgroundStyle,
    currentCanvas,
    currentUnit,
    doesEmbedImage,
    doesEmbedPDF,
    doesFollowLinks,
    doesKeepAspectRatio,
    doesUsePopUpMenu,
    doesUseVariableCursor,
    doesUseXInput,
    getHoodle,
    hoodleModeState,
    hoodleModeStateEither,
    isSaved,
    penInfo,
    settings,
    unitHoodles,
  )
import Hoodle.Type.PageArrangement (ZoomMode (..))
import Hoodle.Type.Window (SplitType (..))
import Hoodle.Widget.Clock (toggleClock)
import Hoodle.Widget.Layer (toggleLayer)
import Hoodle.Widget.PanZoom (togglePanZoom, toggleTouch)
import Hoodle.Widget.Scroll (toggleScroll)
--
import Prelude hiding (mapM_)

-- |
menuEventProcess :: MenuEvent -> MainCoroutine ()
menuEventProcess MenuQuit = do
  xstate <- get
  if view (unitHoodles . currentUnit . isSaved) xstate
    then liftIO Gtk.mainQuit
    else askQuitProgram
menuEventProcess MenuPreviousPage = changePage (\x -> x - 1)
menuEventProcess MenuNextPage = changePage (+ 1)
menuEventProcess MenuFirstPage = changePage (const 0)
menuEventProcess MenuLastPage = do
  totalnumofpages <-
    gets
      ( either (M.size . view gpages) (M.size . view gselAll)
          . hoodleModeStateEither
          . view (unitHoodles . currentUnit . hoodleModeState)
      )
  changePage (const (totalnumofpages - 1))
menuEventProcess MenuNewPageBefore = newPage Nothing PageBefore
menuEventProcess MenuNewPageAfter = newPage Nothing PageAfter
menuEventProcess MenuDeletePage = deleteCurrentPage
menuEventProcess MenuExportPageSVG = exportCurrentPageAsSVG
menuEventProcess MenuNew = addTab (LocalDir Nothing) -- askIfSave fileNew
menuEventProcess MenuAnnotatePDF = askIfSave fileAnnotatePDF
menuEventProcess MenuLoadPNGorJPG = fileLoadPNGorJPG
menuEventProcess MenuLoadSVG = fileLoadSVG
menuEventProcess MenuLoadImageBackground = fileLoadImageBackground
menuEventProcess MenuText = textInput (Just (100, 100)) ""
menuEventProcess MenuEmbedTextSource = embedTextSource
menuEventProcess MenuEditEmbedTextSource = editEmbeddedTextSource
menuEventProcess MenuTextFromSource = textInputFromSource (100, 100)
menuEventProcess MenuLaTeX =
  laTeXInput Nothing (laTeXHeader <> "\n\n" <> laTeXFooter)
menuEventProcess MenuCombineLaTeX = combineLaTeXText
menuEventProcess MenuLaTeXFromSource = laTeXInputFromSource (100, 100)
menuEventProcess MenuUndo = undo
menuEventProcess MenuRedo = redo
menuEventProcess MenuOpen = askIfSave fileOpen
menuEventProcess MenuSave = fileSave
menuEventProcess MenuSaveAs = fileSaveAs
menuEventProcess MenuReload = fileReload
menuEventProcess MenuExport = fileExport
menuEventProcess MenuVersionSave = fileVersionSave
menuEventProcess MenuShowRevisions = fileShowRevisions
menuEventProcess MenuShowUUID = fileShowUUID
menuEventProcess MenuCut = cutSelection
menuEventProcess MenuCopy = copySelection
menuEventProcess MenuPaste = pasteToSelection
menuEventProcess MenuDelete = deleteSelection
menuEventProcess MenuZoomIn = pageZoomChangeRel ZoomIn
menuEventProcess MenuZoomOut = pageZoomChangeRel ZoomOut
menuEventProcess MenuNormalSize = pageZoomChange Original
menuEventProcess MenuPageWidth = pageZoomChange FitWidth
menuEventProcess MenuPageHeight = pageZoomChange FitHeight
menuEventProcess MenuHSplit = eitherSplit SplitHorizontal
menuEventProcess MenuVSplit = eitherSplit SplitVertical
menuEventProcess MenuDelCanvas = deleteCanvas
menuEventProcess MenuNewLayer = makeNewLayer
menuEventProcess MenuNextLayer = gotoNextLayer
menuEventProcess MenuPrevLayer = gotoPrevLayer
menuEventProcess MenuGotoLayer = startGotoLayerAt
menuEventProcess MenuDeleteLayer = deleteCurrentLayer
menuEventProcess MenuUseXInput = do
  _ <- updateFlagFromToggleUI "UXINPUTA" (settings . doesUseXInput)
  return ()
menuEventProcess MenuUseTouch = toggleTouch
menuEventProcess MenuUsePopUpMenu = void $ updateFlagFromToggleUI "POPMENUA" (settings . doesUsePopUpMenu)
menuEventProcess MenuEmbedImage = void $ updateFlagFromToggleUI "EBDIMGA" (settings . doesEmbedImage)
menuEventProcess MenuEmbedPDF = void $ updateFlagFromToggleUI "EBDPDFA" (settings . doesEmbedPDF)
menuEventProcess MenuFollowLinks = void $ updateFlagFromToggleUI "FLWLNKA" (settings . doesFollowLinks)
menuEventProcess MenuKeepAspectRatio = void $ updateFlagFromToggleUI "KEEPRATIOA" (settings . doesKeepAspectRatio)
menuEventProcess MenuUseVariableCursor =
  void $
    updateFlagFromToggleUI "VCURSORA" (settings . doesUseVariableCursor) >> reflectCursor True
menuEventProcess MenuPressureSensitivity = void $ updateFlagFromToggleUI "PRESSRSENSA" (penInfo . variableWidthPen)
menuEventProcess MenuRelaunch = liftIO relaunchApplication
menuEventProcess MenuColorPicker = colorPick
menuEventProcess MenuFullScreen = fullScreen
menuEventProcess MenuAddLink = addLink
menuEventProcess MenuAddAnchor = addAnchor
menuEventProcess MenuListAnchors = listAnchors
menuEventProcess MenuEmbedPredefinedImage = embedPredefinedImage
menuEventProcess MenuEmbedPredefinedImage2 = embedPredefinedImage2
menuEventProcess MenuEmbedPredefinedImage3 = embedPredefinedImage3
menuEventProcess MenuApplyToAllPages = do
  xst <- get
  let bsty = view backgroundStyle xst
      uhdl = view (unitHoodles . currentUnit) xst
      hdl = getHoodle uhdl
      pgs = view gpages hdl
      changeBkg cpage =
        let cbkg = view gbackground cpage
            nbkg
              | isRBkgSmpl cbkg = cbkg {rbkg_style = convertBackgroundStyleToByteString bsty}
              | otherwise = cbkg
         in set gbackground nbkg cpage
      npgs = fmap changeBkg pgs
      nhdl = set gpages npgs hdl
  modeChange ToViewAppendMode
  pureUpdateUhdl (const ((hoodleModeState .~ ViewAppendState nhdl) uhdl))
  invalidateAll
menuEventProcess MenuEmbedAllPDFBkg = embedAllPDFBackground
menuEventProcess MenuTogglePanZoomWidget = togglePanZoom . view (unitHoodles . currentUnit . currentCanvas . _1) =<< get
menuEventProcess MenuToggleLayerWidget = toggleLayer . view (unitHoodles . currentUnit . currentCanvas . _1) =<< get
menuEventProcess MenuToggleClockWidget = toggleClock . view (unitHoodles . currentUnit . currentCanvas . _1) =<< get
menuEventProcess MenuToggleScrollWidget = toggleScroll . view (unitHoodles . currentUnit . currentCanvas . _1) =<< get
menuEventProcess MenuAddTab = addTab (LocalDir Nothing)
menuEventProcess MenuCloseTab = closeTab
menuEventProcess MenuEditNetEmbedTextSource = editNetEmbeddedTextSource
menuEventProcess MenuToggleNetworkEditSource = toggleNetworkEditSource
menuEventProcess MenuLaTeXNetwork =
  laTeXInputNetwork Nothing (laTeXHeader <> "\n\n" <> laTeXFooter)
menuEventProcess m = liftIO $ putStrLn $ "not implemented " ++ show m

-- |
colorPick :: MainCoroutine ()
colorPick = colorPickerBox "Pen Color" >>= mapM_ (\c -> modify (penInfo . currentTool . penColor .~ c))

-- |
colorConvert :: Gtk.Color -> PenColor
colorConvert (Gtk.Color r g b) = ColorRGBA (realToFrac r / 65536.0) (realToFrac g / 65536.0) (realToFrac b / 65536.0) 1.0

-- |
colorPickerBox :: String -> MainCoroutine (Maybe PenColor)
colorPickerBox _msg = do
  xst <- get
  let pcolor = view (penInfo . currentTool . penColor) xst
  doIOaction (action pcolor) >> go
  where
    action pcolor _evhandler = do
      dialog <- Gtk.dialogNew -- Gtk.colorSelectionDialogNew msg
      upper <- fmap Gtk.castToContainer (Gtk.dialogGetContentArea dialog)
      vbox <- Gtk.vBoxNew False 0
      Gtk.containerAdd upper vbox
      csel <- Gtk.colorSelectionNew
      let (r, g, b, _a) = convertPenColorToRGBA pcolor
          color = Gtk.Color (floor (r * 65535.0)) (floor (g * 65535.0)) (floor (b * 65535.0))
      Gtk.colorSelectionSetCurrentColor csel color
      Gtk.boxPackStart vbox csel Gtk.PackGrow 0
      _btnOk <- Gtk.dialogAddButton dialog ("Ok" :: String) Gtk.ResponseOk
      Gtk.widgetShowAll dialog
      res <- Gtk.dialogRun dialog
      mc <- case res of
        Gtk.ResponseOk -> do
          clr <- Gtk.colorSelectionGetCurrentColor csel
          return (Just (colorConvert clr))
        _ -> return Nothing
      Gtk.widgetDestroy dialog
      return (UsrEv (ColorChosen mc))
    go = do
      r <- nextevent
      case r of
        ColorChosen mc -> return mc
        UpdateCanvas cid ->
          -- this is temporary
          invalidateInBBox Nothing Efficient cid >> go
        _ -> go
