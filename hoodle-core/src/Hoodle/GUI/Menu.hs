{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.GUI.Menu 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Construct hoodle menus 
--
-----------------------------------------------------------------------------

module Hoodle.GUI.Menu where

-- from other packages
import           Control.Lens (set)
import           Control.Monad
import qualified Data.Foldable as F (forM_)
import qualified Graphics.UI.Gtk as Gtk
import           System.FilePath
-- from hoodle-platform 
import           Data.Hoodle.Predefined 
-- from this package
import           Hoodle.Coroutine.Callback
import           Hoodle.Type
--
import Paths_hoodle_core

justMenu :: MenuEvent -> Maybe UserEvent
justMenu = Just . Menu 


iconList :: [ (String,Gtk.StockId) ]
iconList = [ ("fullscreen.png" , "myfullscreen")
           , ("pencil.png"     , "mypen")
           , ("eraser.png"     , "myeraser")
           , ("highlighter.png", "myhighlighter") 
           , ("text-tool.png"  , "mytext")
           , ("latex-tool.png" , "mylatex")
           , ("shapes.png"     , "myshapes")
           , ("ruler.png"      , "myruler")
           , ("lasso.png"      , "mylasso")
           , ("rect-select.png", "myrectselect")
           , ("stretch.png"    , "mystretch")
           , ("hand.png"       , "myhand")
           , ("recycled.png"   , "mydefault")
           , ("default-pen.png", "mydefaultpen")
           , ("thin.png"       , "mythin")
           , ("medium.png"     , "mymedium")
           , ("thick.png"      , "mythick")
           , ("black.png"      , "myblack") 
           , ("blue.png"       , "myblue")
           , ("red.png"        , "myred")
           , ("green.png"      , "mygreen")
           , ("gray.png"       , "mygray")
           , ("lightblue.png"  , "mylightblue")
           , ("lightgreen.png" , "mylightgreen")
           , ("magenta.png"    , "mymagenta")
           , ("orange.png"     , "myorange")
           , ("yellow.png"     , "myyellow")
           , ("white.png"      , "mywhite")
           ]

-- | 
viewmods :: [Gtk.RadioActionEntry] 
viewmods = [ Gtk.RadioActionEntry "CONTA" "Continuous" Nothing Nothing Nothing 0
           , Gtk.RadioActionEntry "ONEPAGEA" "One Page" Nothing Nothing Nothing 1
           ]
           
-- | 
pointmods :: [Gtk.RadioActionEntry] 
pointmods = [ Gtk.RadioActionEntry "PENVERYFINEA" "Very fine" Nothing Nothing Nothing 0
            , Gtk.RadioActionEntry "PENFINEA" "Fine" (Just "mythin") Nothing Nothing 1
            
            , Gtk.RadioActionEntry "PENTHICKA" "Thick" (Just "mythick") Nothing Nothing 3 
            , Gtk.RadioActionEntry "PENVERYTHICKA" "Very Thick" Nothing Nothing Nothing 4 
            , Gtk.RadioActionEntry "PENULTRATHICKA" "Ultra Thick" Nothing Nothing Nothing 5   
            , Gtk.RadioActionEntry "PENMEDIUMA" "Medium" (Just "mymedium") Nothing Nothing 2              
--             , Gtk.RadioActionEntry "NOWIDTH" "Unknown" Nothing Nothing Nothing 999 
            ]            

-- | 

penmods :: [Gtk.RadioActionEntry] 
penmods = [ Gtk.RadioActionEntry "PENA"    "Pen"         (Just "mypen")         Nothing Nothing 0 
          , Gtk.RadioActionEntry "ERASERA" "Eraser"      (Just "myeraser")      Nothing Nothing 1
          , Gtk.RadioActionEntry "HIGHLTA" "Highlighter" (Just "myhighlighter") Nothing Nothing 2
--           , Gtk.RadioActionEntry "TEXTA"   "Text"        (Just "mytext")        Nothing Nothing 3 
          , Gtk.RadioActionEntry "SELREGNA" "Select Region"     (Just "mylasso")        Nothing Nothing 4
          , Gtk.RadioActionEntry "SELRECTA" "Select Rectangle" (Just "myrectselect")        Nothing Nothing 5
          , Gtk.RadioActionEntry "VERTSPA" "Vertical Space"    (Just "mystretch")        Nothing Nothing 6
          ]            

--          , Gtk.RadioActionEntry "HANDA"   "Hand Tool"         (Just "myhand")        Nothing Nothing 7


-- | 

colormods :: [Gtk.RadioActionEntry]
colormods = [ Gtk.RadioActionEntry "BLUEA"       "Blue"       (Just "myblue")       Nothing Nothing 1
            , Gtk.RadioActionEntry "REDA"        "Red"        (Just "myred")        Nothing Nothing 2
            , Gtk.RadioActionEntry "GREENA"      "Green"      (Just "mygreen")      Nothing Nothing 3
            , Gtk.RadioActionEntry "GRAYA"       "Gray"       (Just "mygray")       Nothing Nothing 4
            , Gtk.RadioActionEntry "LIGHTBLUEA"  "Lightblue"  (Just "mylightblue")  Nothing Nothing 5     
            , Gtk.RadioActionEntry "LIGHTGREENA" "Lightgreen" (Just "mylightgreen") Nothing Nothing 6
            , Gtk.RadioActionEntry "MAGENTAA"    "Magenta"    (Just "mymagenta")    Nothing Nothing 7
            , Gtk.RadioActionEntry "ORANGEA"     "Orange"     (Just "myorange")     Nothing Nothing 8
            , Gtk.RadioActionEntry "YELLOWA"     "Yellow"     (Just "myyellow")     Nothing Nothing 9
            , Gtk.RadioActionEntry "WHITEA"      "White"      (Just "mywhite")      Nothing Nothing 10
            , Gtk.RadioActionEntry "BLACKA"      "Black"      (Just "myblack")      Nothing Nothing 0              
---             , Gtk.RadioActionEntry "NOCOLOR"     "Unknown"    Nothing Nothing Nothing 999 
            ]

-- | 
bkgstyles :: [Gtk.RadioActionEntry] 
bkgstyles = [ Gtk.RadioActionEntry "BKGGRAPHA" "Graph" Nothing Nothing Nothing 3 
            , Gtk.RadioActionEntry "BKGPLAINA" "Plain" Nothing Nothing Nothing 0
            , Gtk.RadioActionEntry "BKGLINEDA" "Lined" Nothing Nothing Nothing 1
            , Gtk.RadioActionEntry "BKGRULEDA" "Ruled" Nothing Nothing Nothing 2 
            ]

newpagemods :: [Gtk.RadioActionEntry] 
newpagemods = [ Gtk.RadioActionEntry "NEWPAGEPLAINA" "Plain page" Nothing Nothing Nothing 0 
              , Gtk.RadioActionEntry "NEWPAGELASTA"  "Last page"  Nothing Nothing Nothing 1
              , Gtk.RadioActionEntry "NEWPAGECYCLEA" "Cycle page" Nothing Nothing Nothing 2
              ]


-- | 
iconResourceAdd :: Gtk.IconFactory -> FilePath -> (FilePath, Gtk.StockId) -> IO ()
iconResourceAdd iconfac resdir (fp,stid) = do 
  myIconSource <- Gtk.iconSourceNew 
  Gtk.iconSourceSetFilename myIconSource (resdir </> fp)
  Gtk.iconSourceSetSize myIconSource Gtk.IconSizeLargeToolbar
  myIconSourceSmall <- Gtk.iconSourceNew 
  Gtk.iconSourceSetFilename myIconSourceSmall (resdir </> fp)
  Gtk.iconSourceSetSize myIconSource Gtk.IconSizeMenu
  myIconSet <- Gtk.iconSetNew 
  Gtk.iconSetAddSource myIconSet myIconSource 
  Gtk.iconSetAddSource myIconSet myIconSourceSmall
  Gtk.iconFactoryAdd iconfac stid myIconSet

-- | 

actionNewAndRegisterRef :: EventVar                            
                           -> String -> String 
                           -> Maybe String -> Maybe Gtk.StockId
                           -> Maybe UserEvent 
                           -> IO Gtk.Action
actionNewAndRegisterRef evar name label tooltip stockId myevent = do 
    a <- Gtk.actionNew name label tooltip stockId 
    case myevent of 
      Nothing -> return a 
      Just ev -> do 
        a `Gtk.on` Gtk.actionActivated $ do 
          eventHandler evar (UsrEv ev)
        return a

-- | 

getMenuUI :: EventVar -> IO (Gtk.UIManager,UIComponentSignalHandler)
getMenuUI evar = do 
  let actionNewAndRegister = actionNewAndRegisterRef evar  
  -- icons   
  myiconfac <- Gtk.iconFactoryNew 
  Gtk.iconFactoryAddDefault myiconfac 
  resDir <- getDataDir >>= return . (</> "resource") 
  mapM_ (iconResourceAdd myiconfac resDir) iconList 
  fma     <- actionNewAndRegister "FMA"   "File" Nothing Nothing Nothing
  ema     <- actionNewAndRegister "EMA"   "Edit" Nothing Nothing Nothing
  vma     <- actionNewAndRegister "VMA"   "View" Nothing Nothing Nothing
  lma     <- actionNewAndRegister "LMA"   "Layer" Nothing Nothing Nothing
  ima     <- actionNewAndRegister "IMA"   "Embed" Nothing Nothing Nothing
  pma     <- actionNewAndRegister "PMA"   "Page" Nothing Nothing Nothing
  tma     <- actionNewAndRegister "TMA"   "Tool" Nothing Nothing Nothing
  verma   <- actionNewAndRegister "VERMA" "Version" Nothing Nothing Nothing
  oma     <- actionNewAndRegister "OMA"   "Option" Nothing Nothing Nothing
  wma     <- actionNewAndRegister "WMA"   "Window" Nothing Nothing Nothing
  hma     <- actionNewAndRegister "HMA"   "Help" Nothing Nothing Nothing

  ---------------
  -- file menu --
  ---------------
  newa    <- actionNewAndRegister "NEWA"  "New" (Just "Just a Stub") (Just Gtk.stockNew) (justMenu MenuNew)
  opena   <- actionNewAndRegister "OPENA" "Open" (Just "Just a Stub") (Just Gtk.stockOpen) (justMenu MenuOpen)
  savea   <- actionNewAndRegister "SAVEA" "Save" (Just "Just a Stub") (Just Gtk.stockSave) (justMenu MenuSave)
  saveasa <- actionNewAndRegister "SAVEASA" "Save As" (Just "Just a Stub") (Just Gtk.stockSaveAs) (justMenu MenuSaveAs)
  printa  <- actionNewAndRegister "PRINTA" "Print" (Just "Just a Stub") Nothing (justMenu MenuPrint)
  --
  exporta <- actionNewAndRegister "EXPORTA" "Export to PDF" (Just "Just a Stub") Nothing (justMenu MenuExport)
  expsvga <- actionNewAndRegister "EXPSVGA" "Export Current Page to SVG" (Just "Just a Stub") Nothing (justMenu MenuExportPageSVG) 
  --   
  annpdfa <- actionNewAndRegister "ANNPDFA" "Annotate PDF" (Just "Just a Stub") Nothing (justMenu MenuAnnotatePDF)
  --
  reloada <- actionNewAndRegister "RELOADA" "Reload File" (Just "Just a Stub") Nothing (justMenu MenuReload)
  recenta <- actionNewAndRegister "RECENTA" "Recent Document" (Just "Just a Stub") Nothing (justMenu MenuRecentDocument)
  --
  quita   <- actionNewAndRegister "QUITA" "Quit" (Just "Just a Stub") (Just Gtk.stockQuit) (justMenu MenuQuit)

  ---------------
  -- edit menu --
  ---------------
  undoa   <- actionNewAndRegister "UNDOA"   "Undo" (Just "Just a Stub") (Just Gtk.stockUndo) (justMenu MenuUndo)
  redoa   <- actionNewAndRegister "REDOA"   "Redo" (Just "Just a Stub") (Just Gtk.stockRedo) (justMenu MenuRedo)
  cuta    <- actionNewAndRegister "CUTA"    "Cut" (Just "Just a Stub")  (Just Gtk.stockCut) (justMenu MenuCut)
  copya   <- actionNewAndRegister "COPYA"   "Copy" (Just "Just a Stub") (Just Gtk.stockCopy) (justMenu MenuCopy)
  pastea  <- actionNewAndRegister "PASTEA"  "Paste" (Just "Just a Stub") (Just Gtk.stockPaste) (justMenu MenuPaste)
  deletea <- actionNewAndRegister "DELETEA" "Delete" (Just "Just a Stub") (Just Gtk.stockDelete) (justMenu MenuDelete)

  ---------------
  -- view menu --
  ---------------
  togpanzooma <- actionNewAndRegister "TOGPANZOOMA" "Show/Hide Zoom Widget"  (Just "Just a stub") Nothing (justMenu MenuTogglePanZoomWidget)
  togscra <- actionNewAndRegister "TOGSCRA" "Show/Hide Scroll Widget"  (Just "Just a stub") Nothing (justMenu MenuToggleScrollWidget)
  -- 
  zooma     <- actionNewAndRegister "ZOOMA"     "Zoom" (Just "Just a Stub") Nothing Nothing -- (justMenu MenuZoom)
  zmina     <- actionNewAndRegister "ZMINA"     "Zoom In" (Just "Zoom In") (Just Gtk.stockZoomIn) (justMenu MenuZoomIn)
  zmouta    <- actionNewAndRegister "ZMOUTA"    "Zoom Out" (Just "Zoom Out") (Just Gtk.stockZoomOut) (justMenu MenuZoomOut)
  nrmsizea  <- actionNewAndRegister "NRMSIZEA"  "Normal Size" (Just "Normal Size") (Just Gtk.stockZoom100) (justMenu MenuNormalSize)
  pgwdtha   <- actionNewAndRegister "PGWDTHA" "Page Width" (Just "Page Width") (Just Gtk.stockZoomFit) (justMenu MenuPageWidth)
  pgheighta <- actionNewAndRegister "PGHEIGHTA" "Page Height" (Just "Page Height") Nothing (justMenu MenuPageHeight)
  setzma    <- actionNewAndRegister "SETZMA"  "Set Zoom" (Just "Set Zoom") (Just Gtk.stockFind) (justMenu MenuSetZoom)
  -- 
  fscra     <- actionNewAndRegister "FSCRA"     "Full Screen" (Just "Just a Stub") (Just "myfullscreen") (justMenu MenuFullScreen)
  --
  fstpagea  <- actionNewAndRegister "FSTPAGEA"  "First Page" (Just "Just a Stub") (Just Gtk.stockGotoFirst) (justMenu MenuFirstPage)
  prvpagea  <- actionNewAndRegister "PRVPAGEA"  "Previous Page" (Just "Just a Stub") (Just Gtk.stockGoBack) (justMenu MenuPreviousPage)
  nxtpagea  <- actionNewAndRegister "NXTPAGEA"  "Next Page" (Just "Just a Stub") (Just Gtk.stockGoForward) (justMenu MenuNextPage)
  lstpagea  <- actionNewAndRegister "LSTPAGEA"  "Last Page" (Just "Just a Stub") (Just Gtk.stockGotoLast) (justMenu MenuLastPage)
  -- 
  hsplita <- actionNewAndRegister "HSPLITA" "Clone View Horizontally" (Just "horizontal split") Nothing (justMenu MenuHSplit)
  vsplita <- actionNewAndRegister "VSPLITA" "Clone View Vertically" (Just "vertical split") Nothing (justMenu MenuVSplit)
  delcvsa <- actionNewAndRegister "DELCVSA" "Remove Clone View" (Just "delete current canvas") Nothing (justMenu MenuDelCanvas)


  ----------------
  -- layer menu --
  ----------------
  toglayera <- actionNewAndRegister "TOGLAYERA" "Show/Hide Layer Widget"  (Just "Just a stub") Nothing (justMenu MenuToggleLayerWidget)
  -- 
  newlyra <- actionNewAndRegister "NEWLYRA" "New Layer"       (Just "Just a Stub") Nothing (justMenu MenuNewLayer)
  nextlayera <- actionNewAndRegister "NEXTLAYERA" "Next Layer" (Just "Just a Stub") Nothing (justMenu MenuNextLayer)
  prevlayera <- actionNewAndRegister "PREVLAYERA" "Prev Layer" (Just "Just a Stub") Nothing (justMenu MenuPrevLayer)
  gotolayera <- actionNewAndRegister "GOTOLAYERA" "Goto Layer" (Just "Just a Stub") Nothing (justMenu MenuGotoLayer)
  dellyra <- actionNewAndRegister "DELLYRA" "Delete Layer"    (Just "Just a Stub") Nothing (justMenu MenuDeleteLayer)

  ----------------
  -- embed menu --
  ----------------

  ldpnga <- actionNewAndRegister "LDIMGA" "Load PNG or JPG Image" (Just "Just a Stub") Nothing (justMenu MenuLoadPNGorJPG)
  ldsvga <- actionNewAndRegister "LDSVGA" "Load SVG Image" (Just "Just a Stub") Nothing (justMenu MenuLoadSVG)
  ldpreimga <- actionNewAndRegister "LDPREIMGA" "Embed Predefined Image File" (Just "Just a Stub") Nothing (justMenu MenuEmbedPredefinedImage)
  ldpreimg2a <- actionNewAndRegister "LDPREIMG2A" "Embed Predefined Image File 2" (Just "Just a Stub") Nothing (justMenu MenuEmbedPredefinedImage2)
  ldpreimg3a <- actionNewAndRegister "LDPREIMG3A" "Embed Predefined Image File 3" (Just "Just a Stub") Nothing (justMenu MenuEmbedPredefinedImage3)

  texta <- actionNewAndRegister "TEXTA" "Text" (Just "Text") (Just "mytext") (justMenu MenuText)
  textsrca <- actionNewAndRegister "TEXTSRCA" "Embed Text Source" (Just "Just a Stub") Nothing (justMenu MenuEmbedTextSource)
  editsrca <- actionNewAndRegister "EDITSRCA" "Edit text source" (Just "Just a Stub") Nothing (justMenu MenuEditEmbedTextSource)
  editnetsrca <- actionNewAndRegister "EDITNETSRCA" "Network edit text source" (Just "Just a Stub") Nothing (justMenu MenuEditNetEmbedTextSource)

  textfromsrca <- actionNewAndRegister "TEXTFROMSRCA" "Text From Source" (Just "Just a Stub") Nothing (justMenu MenuTextFromSource)


  togglenetsrca <- Gtk.toggleActionNew ("TOGGLENETSRCA" :: String) "Toggle network edit text source" (Just "Just a Stub") Nothing
  togglenetsrca `Gtk.on` Gtk.actionToggled $ do
    eventHandler evar (UsrEv (Menu MenuToggleNetworkEditSource))

  latexa <- actionNewAndRegister "LATEXA" "LaTeX" (Just "Just a Stub") (Just "mylatex") (justMenu MenuLaTeX)
  latexneta <- actionNewAndRegister "LATEXNETA" "LaTeX Network" (Just "Just a Stub") (Just "mylatex") (justMenu MenuLaTeXNetwork)  
  combinelatexa <- actionNewAndRegister "COMBINELATEXA" "Combine LaTeX texts to ..." (Just "Just a Stub") Nothing (justMenu MenuCombineLaTeX)  
  latexfromsrca <- actionNewAndRegister "LATEXFROMSRCA" "LaTeX From Source" (Just "Just a Stub") Nothing (justMenu MenuLaTeXFromSource)   
  updatelatexa <- actionNewAndRegister "UPDATELATEXA" "Update LaTeX" (Just "Just a Stub") Nothing (justMenu MenuUpdateLaTeX)   


  ---------------
  -- page menu --
  ---------------

  newpgba <- actionNewAndRegister "NEWPGBA" "New Page Before" (Just "Just a Stub") Nothing (justMenu MenuNewPageBefore)
  newpgaa <- actionNewAndRegister "NEWPGAA" "New Page After"  (Just "Just a Stub") Nothing (justMenu MenuNewPageAfter)
  newpgea <- actionNewAndRegister "NEWPGEA" "New Page At End" (Just "Just a Stub") Nothing (justMenu MenuNewPageAtEnd)
  delpga  <- actionNewAndRegister "DELPGA"  "Delete Page"     (Just "Just a Stub") Nothing (justMenu MenuDeletePage)

  ppsizea <- actionNewAndRegister "PPSIZEA" "Paper Size"      (Just "Just a Stub") Nothing (justMenu MenuPaperSize)
  ppclra  <- actionNewAndRegister "PPCLRA"  "Paper Color"     (Just "Just a Stub") Nothing (justMenu MenuPaperColor)
  ppstya <- actionNewAndRegister "PPSTYA"   "Paper Style" Nothing Nothing Nothing
  apallpga<- actionNewAndRegister "APALLPGA" "Apply To All Pages" (Just "Just a Stub") Nothing (justMenu MenuApplyToAllPages)
  embedbkgpdfa <- actionNewAndRegister "EMBEDBKGPDFA" "Embed All PDF backgroound" (Just "Just a Stub") Nothing (justMenu MenuEmbedAllPDFBkg)
  defppa  <- actionNewAndRegister "DEFPPA"  "Default Paper" (Just "Just a Stub") Nothing (justMenu MenuDefaultPaper)
  setdefppa <- actionNewAndRegister "SETDEFPPA" "Set As Default" (Just "Just a Stub") Nothing (justMenu MenuSetAsDefaultPaper)
  
  -- tools menu
  linka <- actionNewAndRegister "LINKA" "Add Link" (Just "Add Link") (Just Gtk.stockIndex) (justMenu MenuAddLink)
  anchora <- actionNewAndRegister "ANCHORA" "Add Anchor" (Just "Add Anchor") Nothing (justMenu MenuAddAnchor)
  listanchora <- actionNewAndRegister "LISTANCHORA" "List Anchors" (Just "List Anchors") Nothing (justMenu MenuListAnchors)
  handreca <- actionNewAndRegister "HANDRECA" "Hoodlet load via Handwriting Recognition" (Just "Just a Stub") (Just "myshapes") (justMenu MenuHandwritingRecognitionDialog)
  
  clra      <- actionNewAndRegister "CLRA" "Color" (Just "Just a Stub") Nothing Nothing
  clrpcka   <- actionNewAndRegister "CLRPCKA" "Color Picker.." (Just "Just a Stub") (Just Gtk.stockSelectColor) (justMenu MenuColorPicker ) 
  penopta   <- actionNewAndRegister "PENOPTA" "Pen Options" (Just "Just a Stub") Nothing (justMenu MenuPenOptions)
  erasropta <- actionNewAndRegister "ERASROPTA" "Eraser Options" (Just "Just a Stub") Nothing (justMenu MenuEraserOptions)
  hiltropta <- actionNewAndRegister "HILTROPTA" "Highlighter Options" (Just "Just a Stub") Nothing (justMenu MenuHighlighterOptions)
  txtfnta   <- actionNewAndRegister "TXTFNTA" "Text Font" (Just "Just a Stub") Nothing (justMenu MenuTextFont)
  defpena   <- actionNewAndRegister "DEFPENA" "Default Pen" (Just "Just a Stub") (Just "mydefaultpen") (justMenu MenuDefaultPen)
  defersra  <- actionNewAndRegister "DEFERSRA" "Default Eraser" (Just "Just a Stub") Nothing (justMenu MenuDefaultEraser)
  defhiltra <- actionNewAndRegister "DEFHILTRA" "Default Highlighter" (Just "Just a Stub") Nothing (justMenu MenuDefaultHighlighter)
  deftxta   <- actionNewAndRegister "DEFTXTA" "Default Text" (Just "Just a Stub") Nothing (justMenu MenuDefaultText)
  setdefopta <- actionNewAndRegister "SETDEFOPTA" "Set As Default" (Just "Just a Stub") Nothing (justMenu MenuSetAsDefaultOption)



  ------------------
  -- version menu --
  ------------------

  synca <- actionNewAndRegister "SYNCA" "Start Sync" (Just "Just a Stub") Nothing (justMenu MenuStartSync)  
  versiona <- actionNewAndRegister "VERSIONA" "Save Version" (Just "Just a Stub") Nothing (justMenu MenuVersionSave)
  showreva <- actionNewAndRegister "SHOWREVA" "Show Revisions" (Just "Just a Stub") Nothing (justMenu MenuShowRevisions)  
  showida <- actionNewAndRegister "SHOWIDA" "Show UUID" (Just "Just a Stub") Nothing (justMenu MenuShowUUID)    


  -----------------  
  -- option menu --
  -----------------
  uxinputa <- Gtk.toggleActionNew ("UXINPUTA" :: String) "Use XInput" (Just "Just a Stub") Nothing 
  uxinputa `Gtk.on` Gtk.actionToggled $ do 
    eventHandler evar (UsrEv (Menu MenuUseXInput))
  -- handa <- actionNewAndRegister "HANDA" "Use Touch" (Just "Use touch") (Just "myhand") (justMenu MenuUseTouch)    
  handa     <- Gtk.toggleActionNew ("HANDA" :: String) "Use Touch" (Just "Toggle touch") (Just "myhand") 
  handa `Gtk.on` Gtk.actionToggled $ do 
    eventHandler evar (UsrEv (Menu MenuUseTouch))
  -- smthscra <- Gtk.toggleActionNew ("SMTHSCRA" :: String) "Smooth Scrolling" (Just "Just a stub") Nothing
  -- smthscra `Gtk.on` Gtk.actionToggled $ do 
  --   eventHandler evar (UsrEv (Menu MenuSmoothScroll))
  popmenua <- Gtk.toggleActionNew ("POPMENUA" :: String) "Use Popup Menu" (Just "Just a stub") Nothing
  popmenua `Gtk.on` Gtk.actionToggled $ do 
    eventHandler evar (UsrEv (Menu MenuUsePopUpMenu))
  ebdimga <- Gtk.toggleActionNew ("EBDIMGA" :: String) "Embed PNG/JPG Image" (Just "Just a stub") Nothing
  ebdimga `Gtk.on` Gtk.actionToggled $ do 
    eventHandler evar (UsrEv (Menu MenuEmbedImage))
  ebdpdfa <- Gtk.toggleActionNew ("EBDPDFA" :: String) "Embed PDF" (Just "Just a stub") Nothing
  ebdpdfa `Gtk.on` Gtk.actionToggled $ do 
    eventHandler evar (UsrEv (Menu MenuEmbedPDF))
  flwlnka <- Gtk.toggleActionNew ("FLWLNKA" :: String) "Follow Links" (Just "Just a stub") Nothing
  flwlnka `Gtk.on` Gtk.actionToggled $ do 
    eventHandler evar (UsrEv (Menu MenuFollowLinks))    
  keepratioa <- Gtk.toggleActionNew ("KEEPRATIOA" :: String) "Keep Aspect Ratio" (Just "Just a stub") Nothing
  keepratioa `Gtk.on` Gtk.actionToggled $ do 
    eventHandler evar (UsrEv (Menu MenuKeepAspectRatio))
  vcursora <- Gtk.toggleActionNew ("VCURSORA" :: String) "Use Variable Cursor" (Just "Just a stub") Nothing
  vcursora `Gtk.on` Gtk.actionToggled $ do 
    eventHandler evar (UsrEv (Menu MenuUseVariableCursor))
  -- temporary implementation (later will be as submenus with toggle action. appropriate reflection)
  togclocka <- actionNewAndRegister "TOGCLOCKA" "Toggle Clock Widget"  (Just "Just a stub") Nothing (justMenu MenuToggleClockWidget)
    
  pressrsensa <- Gtk.toggleActionNew ("PRESSRSENSA" :: String) "Pressure Sensitivity" (Just "Just a Stub") Nothing 
  pressrsensa `Gtk.on` Gtk.actionToggled $ do 
    eventHandler evar (UsrEv (Menu MenuPressureSensitivity))

  newpagemoda <- actionNewAndRegister "NEWPAGEMODEA" "New page mode" Nothing Nothing Nothing

  relauncha <- actionNewAndRegister "RELAUNCHA" "Relaunch Application" (Just "Just a Stub") Nothing (justMenu MenuRelaunch)
  huba <- actionNewAndRegister "HUBA" "Hub" (Just "Just a Stub") Nothing (justMenu MenuHub)
  hubsocketa <- actionNewAndRegister "HUBSOCKETA" "Hub Socket" (Just "Just a Stub") Nothing (justMenu MenuHubSocket)


  -- window menu
  addtaba <- actionNewAndRegister "ADDTABA" "Add new tab" (Just "Just a Stub") Nothing (justMenu MenuAddTab)
  -- nexttaba <- actionNewAndRegister "NEXTTABA" "Go to next tab" (Just "Just a Stub") Nothing (justMenu MenuNextTab)
  closetaba <- actionNewAndRegister "CLOSETABA" "Close current tab" (Just "Just a Stub") Nothing (justMenu MenuCloseTab)



  -- help menu 
  abouta <- actionNewAndRegister "ABOUTA" "About" (Just "Just a Stub") Nothing (justMenu MenuAbout)

  -- others
  defaulta <- actionNewAndRegister "DEFAULTA" "Default" (Just "Default") (Just "mydefault") (justMenu MenuDefault)
  
  agr <- Gtk.actionGroupNew ("AGR" :: String)
  mapM_ (Gtk.actionGroupAddAction agr) 
        [fma,ema,vma,lma,ima,pma,tma,verma,oma,wma,hma]
  mapM_ (Gtk.actionGroupAddAction agr)   
        [ undoa, redoa, cuta, copya, pastea, deletea ] 
  mapM_ (\act -> Gtk.actionGroupAddActionWithAccel agr act (Nothing :: Maybe String))   
        [ newa, annpdfa, opena, savea, saveasa
        , reloada, recenta, printa, exporta, synca, versiona, showreva, showida, quita
        , fscra, zooma, zmina, zmouta, nrmsizea, pgwdtha, pgheighta, setzma
        , fstpagea, prvpagea, nxtpagea, lstpagea 
        , hsplita, vsplita, delcvsa
        , newpgba, newpgaa, newpgea, delpga, expsvga, newlyra, nextlayera, prevlayera, gotolayera, dellyra, ppsizea, ppclra
        , ppstya 
        , apallpga, embedbkgpdfa, defppa, setdefppa
        , ldpnga, ldsvga, texta, textsrca, editsrca, editnetsrca, textfromsrca
        , latexa, latexneta, combinelatexa, latexfromsrca, updatelatexa
        , ldpreimga, ldpreimg2a, ldpreimg3a
        , linka, anchora, listanchora, handreca, clra, clrpcka, penopta 
        , erasropta, hiltropta, txtfnta, defpena, defersra, defhiltra, deftxta
        , setdefopta
        , togpanzooma, togscra, toglayera, togclocka, newpagemoda, relauncha
        , huba, hubsocketa
        , addtaba, {- nexttaba, -} closetaba
        , abouta 
        , defaulta         
        ] 
    
  mapM_ (Gtk.actionGroupAddAction agr) 
    [ togglenetsrca, uxinputa, handa, popmenua, ebdimga, ebdpdfa, flwlnka
    , keepratioa, pressrsensa, vcursora ]

  mpgmodconnid <- 
    actionGroupAddRadioActionsAndGetConnID agr viewmods 0 (assignViewMode evar)
  _mpointconnid <- 
    actionGroupAddRadioActionsAndGetConnID agr pointmods 0 (assignPoint evar)
  mpenmodconnid <- 
    actionGroupAddRadioActionsAndGetConnID agr penmods   0 (assignPenMode evar)
  mcolorconnid <-  
    actionGroupAddRadioActionsAndGetConnID agr colormods 0 (assignColor evar) 
  _mbkgstyconnid <-
    actionGroupAddRadioActionsAndGetConnID agr bkgstyles 2 (assignBkgStyle evar)
  mnpgmodconnid <- 
    actionGroupAddRadioActionsAndGetConnID agr newpagemods 0 (assignNewPageMode evar)

  
  let disabledActions = 
        [ recenta, printa
        , cuta, copya, deletea
        ,  setzma
        , newpgea, ppsizea, ppclra
        , defppa, setdefppa
        , erasropta, hiltropta, txtfnta, defpena, defersra, defhiltra, deftxta
        , setdefopta
        , abouta
        , defaulta         
        ] 
      enabledActions = 
        [ opena, savea, saveasa, reloada, versiona, showreva, showida, quita
        , pastea, fstpagea, prvpagea, nxtpagea, lstpagea
        , clra, penopta, zooma, nrmsizea, pgwdtha, texta  
        , newpagemoda, relauncha, huba, hubsocketa
        ]
  --
  mapM_ (\x->Gtk.actionSetSensitive x True) enabledActions  
  mapM_ (\x->Gtk.actionSetSensitive x False) disabledActions
  --
  -- 
  -- radio actions
  --
  ui <- Gtk.uiManagerNew
  
  uiDecl <- readFile (resDir </> "menu.xml")   
<<<<<<< HEAD
  Gtk.uiManagerAddUiFromString ui uiDecl
  Gtk.uiManagerInsertActionGroup ui agr 0 
  Just ra2 <- Gtk.actionGroupGetAction agr ("PENFINEA" :: String)
  Gtk.set (Gtk.castToRadioAction ra2) [Gtk.radioActionCurrentValue Gtk.:= 2]
  Just ra3 <- Gtk.actionGroupGetAction agr ("SELREGNA" :: String)
  Gtk.actionSetSensitive ra3 True 
  Just ra4 <- Gtk.actionGroupGetAction agr ("VERTSPA" :: String)
  Gtk.actionSetSensitive ra4 True
  Just ra6 <- Gtk.actionGroupGetAction agr ("CONTA" :: String)
  Gtk.actionSetSensitive ra6 True
  Just _ra7 <- Gtk.actionGroupGetAction agr ("PENA" :: String)
  Gtk.actionSetSensitive ra6 True  
  Just toolbar1 <- Gtk.uiManagerGetWidget ui ("/ui/toolbar1" :: String)
  Gtk.toolbarSetStyle (Gtk.castToToolbar toolbar1) Gtk.ToolbarIcons 
#ifndef GTK3
  Gtk.toolbarSetIconSize (Gtk.castToToolbar toolbar1) Gtk.IconSizeSmallToolbar
#endif // not GTK3
  Just toolbar2 <- Gtk.uiManagerGetWidget ui ("/ui/toolbar2" :: String)
  Gtk.toolbarSetStyle (Gtk.castToToolbar toolbar2) Gtk.ToolbarIcons 
#ifndef GTK3
  Gtk.toolbarSetIconSize (Gtk.castToToolbar toolbar2) Gtk.IconSizeSmallToolbar  
#endif // not GTK3
    
  let uicomponentsignalhandler = set penModeSignal mpenmodconnid 
                                 . set pageModeSignal mpgmodconnid 
                                 . set penColorSignal mcolorconnid
                                 . set newPageModeSignal mnpgmodconnid
                                 $ defaultUIComponentSignalHandler 
  return (ui,uicomponentsignalhandler)   


-- |
actionGroupAddRadioActionsAndGetConnID :: Gtk.ActionGroup 
                                       -> [Gtk.RadioActionEntry]
                                       -> Int  
                                       -> (Gtk.RadioAction -> IO ()) 
                                       -> IO (Maybe (Gtk.ConnectId Gtk.RadioAction))
actionGroupAddRadioActionsAndGetConnID self entries _value onChange = do 
  mgroup <- foldM 
    (\mgroup (n,Gtk.RadioActionEntry name label stockId accelerator tooltip value) -> do
     action <- Gtk.radioActionNew name label tooltip stockId value
     F.forM_ mgroup $ \gr -> Gtk.radioActionSetGroup action gr
     when (n==value) (Gtk.toggleActionSetActive action True)
     Gtk.actionGroupAddActionWithAccel self action accelerator
     return (Just action))
    Nothing (zip [0..] entries)
  case mgroup of 
    Nothing -> return Nothing 
    Just gr -> do 
      connid <- (gr `Gtk.on` Gtk.radioActionChanged) onChange
      return (Just connid)


-- | 
assignViewMode :: EventVar -> Gtk.RadioAction -> IO ()
assignViewMode evar a = viewModeToUserEvent a >>= eventHandler evar . UsrEv
    
-- | 
assignPenMode :: EventVar -> Gtk.RadioAction -> IO ()
assignPenMode evar a = do 
    v <- Gtk.radioActionGetCurrentValue a
    eventHandler evar (UsrEv (AssignPenMode (int2PenType v)))

      
-- | 
assignColor :: EventVar -> Gtk.RadioAction -> IO () 
assignColor evar a = do 
    v <- Gtk.radioActionGetCurrentValue a
    let c = int2Color v
    eventHandler evar (UsrEv (PenColorChanged c))

-- | 
assignPoint :: EventVar -> Gtk.RadioAction -> IO ()  
assignPoint evar a = do 
    v <- Gtk.radioActionGetCurrentValue a
    eventHandler evar (UsrEv (PenWidthChanged v))


-- | 
assignBkgStyle :: EventVar -> Gtk.RadioAction -> IO ()
assignBkgStyle evar a = do 
    v <- Gtk.radioActionGetCurrentValue a 
    let sty = int2BkgStyle v 
    eventHandler evar (UsrEv (BackgroundStyleChanged sty))

-- | 
assignNewPageMode :: EventVar -> Gtk.RadioAction -> IO ()
assignNewPageMode evar a = do 
    v <- Gtk.radioActionGetCurrentValue a
    eventHandler evar (UsrEv (AssignNewPageMode (int2NewPageMode v)))


-- | 
int2PenType :: Int -> Either PenType SelectType 
int2PenType 0 = Left PenWork
int2PenType 1 = Left EraserWork
int2PenType 2 = Left HighlighterWork
-- int2PenType 3 = Left TextWork 
int2PenType 4 = Right SelectLassoWork
int2PenType 5 = Right SelectRectangleWork
int2PenType 6 = Left VerticalSpaceWork
-- int2PenType 7 = Right SelectHandToolWork
int2PenType _ = error "No such pentype"

-- | 
penType2Int :: Either PenType SelectType -> Int 
penType2Int (Left PenWork)              = 0
penType2Int (Left EraserWork)           = 1
penType2Int (Left HighlighterWork)      = 2 
penType2Int (Left VerticalSpaceWork)    = 6
penType2Int (Right SelectLassoWork)    = 4 
penType2Int (Right SelectRectangleWork) = 5 
penType2Int _ = 100
-- penType2Int (Right SelectHandToolWork)  = 7 


-- | 
int2Point :: PenType -> Int -> Double 
int2Point PenWork 0 = predefined_veryfine 
int2Point PenWork 1 = predefined_fine
int2Point PenWork 2 = predefined_medium
int2Point PenWork 3 = predefined_thick
int2Point PenWork 4 = predefined_verythick
int2Point PenWork 5 = predefined_ultrathick
int2Point HighlighterWork 0 = predefined_highlighter_veryfine
int2Point HighlighterWork 1 = predefined_highlighter_fine
int2Point HighlighterWork 2 = predefined_highlighter_medium
int2Point HighlighterWork 3 = predefined_highlighter_thick
int2Point HighlighterWork 4 = predefined_highlighter_verythick
int2Point HighlighterWork 5 = predefined_highlighter_ultrathick
int2Point EraserWork 0 = predefined_eraser_veryfine
int2Point EraserWork 1 = predefined_eraser_fine
int2Point EraserWork 2 = predefined_eraser_medium
int2Point EraserWork 3 = predefined_eraser_thick
int2Point EraserWork 4 = predefined_eraser_verythick
int2Point EraserWork 5 = predefined_eraser_ultrathick
int2Point _ _ = error "No such point"

similarTo :: Double -> Double -> Bool
similarTo v w = (v < w + eps) && (v > w - eps) 
  where eps = 1e-2

-- | 
point2Int :: PenType -> Double -> Int 
point2Int PenWork v  
  | v `similarTo` predefined_veryfine   = 0
  | v `similarTo` predefined_fine       = 1
  | v `similarTo` predefined_medium     = 2
  | v `similarTo` predefined_thick      = 3 
  | v `similarTo` predefined_verythick  = 4
  | v `similarTo` predefined_ultrathick = 5
point2Int HighlighterWork v 
  | v `similarTo` predefined_highlighter_fine       = 1
  | v `similarTo` predefined_highlighter_veryfine   = 0
  | v `similarTo` predefined_highlighter_medium     = 2
  | v `similarTo` predefined_highlighter_thick      = 3 
  | v `similarTo` predefined_highlighter_verythick  = 4
  | v `similarTo` predefined_highlighter_ultrathick = 5  
point2Int EraserWork v
  | v `similarTo` predefined_eraser_veryfine   = 0
  | v `similarTo` predefined_eraser_fine       = 1
  | v `similarTo` predefined_eraser_medium     = 2
  | v `similarTo` predefined_eraser_thick      = 3 
  | v `similarTo` predefined_eraser_verythick  = 4
  | v `similarTo` predefined_eraser_ultrathick = 5  
point2Int _ _  = 0 -- for the time being 


-- | 
int2Color :: Int -> PenColor
int2Color 0  = ColorBlack 
int2Color 1  = ColorBlue
int2Color 2  = ColorRed
int2Color 3  = ColorGreen
int2Color 4  = ColorGray
int2Color 5  = ColorLightBlue
int2Color 6  = ColorLightGreen
int2Color 7  = ColorMagenta
int2Color 8  = ColorOrange
int2Color 9  = ColorYellow
int2Color 10 = ColorWhite
int2Color _ = error "No such color"


color2Int :: PenColor -> Int 
color2Int ColorBlack      = 0
color2Int ColorBlue       = 1
color2Int ColorRed        = 2
color2Int ColorGreen      = 3
color2Int ColorGray       = 4
color2Int ColorLightBlue  = 5
color2Int ColorLightGreen = 6
color2Int ColorMagenta    = 7 
color2Int ColorOrange     = 8 
color2Int ColorYellow     = 9
color2Int ColorWhite      = 10
color2Int _ = 0  -- just for the time being 


int2BkgStyle :: Int -> BackgroundStyle 
int2BkgStyle 0 = BkgStylePlain 
int2BkgStyle 1 = BkgStyleLined
int2BkgStyle 2 = BkgStyleRuled
int2BkgStyle 3 = BkgStyleGraph
int2BkgStyle _ = BkgStyleRuled 


-- | 
int2NewPageMode :: Int -> NewPageModeType
int2NewPageMode 0 = NPPlain
int2NewPageMode 1 = NPLast
int2NewPageMode 2 = NPCycle
int2NewPageMode _ = error "No such new page mode"

-- |
newPageMode2Int :: NewPageModeType -> Int
newPageMode2Int NPPlain = 0
newPageMode2Int NPLast  = 1
newPageMode2Int NPCycle = 2
