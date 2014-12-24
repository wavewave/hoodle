{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Default.Menu
-- Copyright   : (c) 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Default.Menu where

import           Control.Applicative
import           Control.Lens (_1,view,set,(.~))
import           Control.Monad.IO.Class
import           Control.Monad.State hiding (mapM_)
import           Data.Foldable (mapM_)
import qualified Data.IntMap as M
import           Data.Monoid
import qualified Data.Text as T
import qualified Graphics.UI.Gtk as Gtk
--
import           Data.Hoodle.Generic
import           Data.Hoodle.Select
import           Graphics.Hoodle.Render.Type
--
import           Hoodle.Accessor
import           Hoodle.Coroutine.Commit
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.File
import           Hoodle.Coroutine.HandwritingRecognition
import           Hoodle.Coroutine.LaTeX
import           Hoodle.Coroutine.Layer
import           Hoodle.Coroutine.Link
import           Hoodle.Coroutine.Mode
import           Hoodle.Coroutine.Page
import           Hoodle.Coroutine.Select.Clipboard
import           Hoodle.Coroutine.TextInput
import           Hoodle.Coroutine.Window
import           Hoodle.GUI.Reflect
import           Hoodle.Script
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.Window
import           Hoodle.Widget.Clock
import           Hoodle.Widget.Layer
import           Hoodle.Widget.PanZoom
import           Hoodle.Widget.Scroll
#ifdef HUB
import           Hoodle.Coroutine.Hub
import           Hoodle.Coroutine.Socket
#endif
--
import Prelude hiding (mapM_)



-- |
menuEventProcess :: MenuEvent -> MainCoroutine () 
menuEventProcess MenuQuit = do 
    xstate <- get
    if view (unitHoodles.currentUnit.isSaved) xstate 
      then liftIO $ Gtk.mainQuit
      else askQuitProgram
menuEventProcess MenuPreviousPage = changePage (\x->x-1)
menuEventProcess MenuNextPage =  changePage (+1)
menuEventProcess MenuFirstPage = changePage (const 0)
menuEventProcess MenuLastPage = do 
    totalnumofpages <- (either (M.size. view gpages) (M.size . view gselAll) 
                        . hoodleModeStateEither . view (unitHoodles.currentUnit.hoodleModeState)) <$> get 
    changePage (const (totalnumofpages-1))
menuEventProcess MenuNewPageBefore = newPage PageBefore 
menuEventProcess MenuNewPageAfter = newPage PageAfter
menuEventProcess MenuDeletePage = deleteCurrentPage
menuEventProcess MenuExportPageSVG = exportCurrentPageAsSVG 
menuEventProcess MenuNew  = addTab Nothing -- askIfSave fileNew 
menuEventProcess MenuAnnotatePDF = askIfSave fileAnnotatePDF
menuEventProcess MenuLoadPNGorJPG = fileLoadPNGorJPG
menuEventProcess MenuLoadSVG = fileLoadSVG
menuEventProcess MenuText = textInput (Just (100,100)) "" 
menuEventProcess MenuEmbedTextSource = embedTextSource
menuEventProcess MenuEditEmbedTextSource = editEmbeddedTextSource
menuEventProcess MenuTextFromSource = textInputFromSource (100,100)
menuEventProcess MenuLaTeX = 
    laTeXInput Nothing (laTeXHeader <> "\n\n" <> laTeXFooter)
menuEventProcess MenuCombineLaTeX = combineLaTeXText 
menuEventProcess MenuLaTeXFromSource = laTeXInputFromSource (100,100)
-- menuEventProcess MenuUpdateLaTeX = updateLaTeX
menuEventProcess MenuUndo = undo 
menuEventProcess MenuRedo = redo
menuEventProcess MenuOpen = askIfSave fileOpen
menuEventProcess MenuSave = fileSave 
menuEventProcess MenuSaveAs = fileSaveAs
menuEventProcess MenuReload = fileReload 
menuEventProcess MenuExport = fileExport 
menuEventProcess MenuStartSync = fileStartSync
menuEventProcess MenuVersionSave = fileVersionSave 
menuEventProcess MenuShowRevisions = fileShowRevisions
menuEventProcess MenuShowUUID = fileShowUUID
-- 
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
    liftIO $ putStrLn "MenuUseXInput called"
    uhdl <- view (unitHoodles.currentUnit) <$> get 
    let cmap = view cvsInfoMap uhdl
        canvases = map (getDrawAreaFromBox) . M.elems $ cmap 
    updateFlagFromToggleUI "UXINPUTA" (settings.doesUseXInput) >>= \b -> 
#ifdef GTK3      
      return ()
#else
      if b
        then mapM_ (\x->liftIO $ Gtk.widgetSetExtensionEvents x [Gtk.ExtensionEventsAll]) canvases
        else mapM_ (\x->liftIO $ Gtk.widgetSetExtensionEvents x [Gtk.ExtensionEventsNone] ) canvases
#endif
menuEventProcess MenuUseTouch = toggleTouch
menuEventProcess MenuUsePopUpMenu = updateFlagFromToggleUI "POPMENUA" (settings.doesUsePopUpMenu) >> return ()
menuEventProcess MenuEmbedImage = updateFlagFromToggleUI "EBDIMGA" (settings.doesEmbedImage) >> return ()
menuEventProcess MenuEmbedPDF = updateFlagFromToggleUI "EBDPDFA" (settings.doesEmbedPDF) >> return ()
menuEventProcess MenuFollowLinks = updateFlagFromToggleUI "FLWLNKA" (settings.doesFollowLinks) >> return ()
menuEventProcess MenuKeepAspectRatio = updateFlagFromToggleUI "KEEPRATIOA" (settings.doesKeepAspectRatio) >> return ()
menuEventProcess MenuUseVariableCursor = updateFlagFromToggleUI "VCURSORA" (settings.doesUseVariableCursor) >> reflectCursor >> return ()
menuEventProcess MenuPressureSensitivity = updateFlagFromToggleUI "PRESSRSENSA" (penInfo.variableWidthPen) >> return ()  
#ifdef DYRE
menuEventProcess MenuRelaunch = liftIO $ relaunchApplication
#endif
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
        uhdl = view (unitHoodles.currentUnit) xst
        hdl = getHoodle uhdl
        pgs = view gpages hdl 
        changeBkg cpage = 
          let cbkg = view gbackground cpage
              nbkg 
                | isRBkgSmpl cbkg = cbkg { rbkg_style = convertBackgroundStyleToByteString bsty }
                | otherwise = cbkg 
          in set gbackground nbkg cpage 
        npgs = fmap changeBkg pgs 
        nhdl = set gpages npgs hdl 
    modeChange ToViewAppendMode     
    pureUpdateUhdl (const ((hoodleModeState .~ ViewAppendState nhdl) uhdl))
    invalidateAll 
menuEventProcess MenuEmbedAllPDFBkg = embedAllPDFBackground
menuEventProcess MenuTogglePanZoomWidget = togglePanZoom . view (unitHoodles.currentUnit.currentCanvas._1) =<< get 
menuEventProcess MenuToggleLayerWidget = toggleLayer . view (unitHoodles.currentUnit.currentCanvas._1) =<< get 
menuEventProcess MenuToggleClockWidget = toggleClock . view (unitHoodles.currentUnit.currentCanvas._1) =<< get
menuEventProcess MenuToggleScrollWidget = toggleScroll . view (unitHoodles.currentUnit.currentCanvas._1) =<< get
menuEventProcess MenuHandwritingRecognitionDialog = 
    handwritingRecognitionDialog >>= mapM_ (\(b,txt) -> when b $ embedHoodlet (T.unpack txt)) 
menuEventProcess MenuAddTab = addTab Nothing
-- menuEventProcess MenuNextTab = nextTab
menuEventProcess MenuCloseTab = closeTab
#ifdef HUB
menuEventProcess MenuEditNetEmbedTextSource = editNetEmbeddedTextSource
menuEventProcess MenuToggleNetworkEditSource = toggleNetworkEditSource
menuEventProcess MenuLaTeXNetwork = 
    laTeXInputNetwork Nothing (laTeXHeader <> "\n\n" <> laTeXFooter)
menuEventProcess MenuHub = hubUpload
-- menuEventProcess MenuHubSocket = socketConnect
#endif
menuEventProcess m = liftIO $ putStrLn $ "not implemented " ++ show m 

-- | 
colorPick :: MainCoroutine () 
colorPick = colorPickerBox "Pen Color" >>= mapM_ (\c->modify (penInfo.currentTool.penColor .~ c))

-- | 
colorConvert :: Gtk.Color -> PenColor 
colorConvert (Gtk.Color r g b) = ColorRGBA (realToFrac r/65536.0) (realToFrac g/65536.0) (realToFrac b/65536.0) 1.0 
-- | 
colorPickerBox :: String -> MainCoroutine (Maybe PenColor) 
colorPickerBox msg = do 
#ifdef GTK3 
    -- color selection dialog is incomplete in gtk3
    return Nothing
#else
    xst <- get 
    let pcolor = view (penInfo.currentTool.penColor) xst   
    doIOaction (action pcolor) >> go
  where 
    action pcolor _evhandler = do 
      dialog <- Gtk.colorSelectionDialogNew msg
      csel <- Gtk.colorSelectionDialogGetColor dialog
      let (r,g,b,_a) =  convertPenColorToRGBA pcolor 
          color = Gtk.Color (floor (r*65535.0)) (floor (g*65535.0)) (floor (b*65535.0))

      Gtk.colorSelectionSetCurrentColor csel color
      res <- Gtk.dialogRun dialog 
      mc <- case res of 
              Gtk.ResponseOk -> do 
                   clrsel <- Gtk.colorSelectionDialogGetColor dialog 
                   clr <- Gtk.colorSelectionGetCurrentColor clrsel     
                   return (Just (colorConvert clr))
              _ -> return Nothing 
      Gtk.widgetDestroy dialog 
      return (UsrEv (ColorChosen mc))
    go = do r <- nextevent                   
            case r of 
              ColorChosen mc -> return mc 
              UpdateCanvas cid -> -- this is temporary
                invalidateInBBox Nothing Efficient cid >> go
              _ -> go 
#endif
