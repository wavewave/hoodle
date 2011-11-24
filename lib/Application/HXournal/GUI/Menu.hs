{-# LANGUAGE QuasiQuotes #-}

module Application.HXournal.GUI.Menu where

import Application.HXournal.Util.Verbatim
import Application.HXournal.Coroutine
import Application.HXournal.Type

import Control.Monad.IO.Class
import Control.Monad.Coroutine.SuspensionFunctors
import Data.IORef
import qualified Data.Map as M
import Data.Maybe

import Graphics.UI.Gtk

import System.FilePath

import Text.Xournal.Predefined 
import Paths_hxournal

uiDecl :: String 
uiDecl = [verbatim|<ui> 
  <menubar>
    <menu action="FMA">
       <menuitem action="NEWA" />
       <menuitem action="ANNPDFA" /> 
       <menuitem action="OPENA" />                        
       <menuitem action="SAVEA" />                        
       <menuitem action="SAVEASA" />                        
       <separator />
       <menuitem action="RECENTA" />                        
       <separator /> 
       <menuitem action="PRINTA" />                        
       <menuitem action="EXPORTA" />
       <separator /> 
       <menuitem action="QUITA" />
    </menu>
    <menu action="EMA">
       <menuitem action="UNDOA" />
       <menuitem action="REDOA" />                       
       <separator />
       <menuitem action="CUTA" />                        
       <menuitem action="COPYA" />                        
       <menuitem action="PASTEA" />                        
       <menuitem action="DELETEA" />                        
    </menu>
    <menu action="VMA">
       <menuitem action="CONTA" />
       <menuitem action="ONEPAGEA" />
       <separator />
       <menuitem action="FSCRA" />
       <separator />
       <menu action="ZOOMA" >
         <menuitem action="ZMINA" />
         <menuitem action="ZMOUTA" />                          
         <menuitem action="NRMSIZEA" />                          
         <menuitem action="PGWDTHA" />                          
         <menuitem action="SETZMA" />                          
       </menu>
       <separator />
       <menuitem action="FSTPAGEA" />
       <menuitem action="PRVPAGEA" />
       <menuitem action="NXTPAGEA" />
       <menuitem action="LSTPAGEA" />
       <separator />
       <menuitem action="SHWLAYERA" />
       <menuitem action="HIDLAYERA" />
    </menu>
    <menu action="JMA">
       <menuitem action="NEWPGBA" />
       <menuitem action="NEWPGAA" />
       <menuitem action="NEWPGEA" />
       <menuitem action="DELPGA" />       
       <separator />
       <menuitem action="NEWLYRA" />
       <menuitem action="DELLYRA" />       
       <separator />
       <menuitem action="PPSIZEA" />
       <menuitem action="PPCLRA" />
       <menuitem action="PPSTYA" />
       <menuitem action="APALLPGA" />
       <separator />
       <menuitem action="LDBKGA" />
       <menuitem action="BKGSCRSHTA" />
       <separator />
       <menuitem action="DEFPPA" />                        
       <menuitem action="SETDEFPPA" />    
    </menu>
    <menu action="TMA">
       <menuitem action="PENA" />
       <menuitem action="ERASERA" />
       <menuitem action="HIGHLTA" />
       <menuitem action="TEXTA" />       
       <separator />
       <menuitem action="SHPRECA" />
       <menuitem action="RULERA" />       
       <separator />
       <menuitem action="SELREGNA" />
       <menuitem action="SELRECTA" />
       <menuitem action="VERTSPA" />
       <menuitem action="HANDA" />
       <separator />
       <menu action="CLRA"> 
         <menuitem action="BLACKA" />     
         <menuitem action="BLUEA" />                          
         <menuitem action="REDA" />                          
         <menuitem action="GREENA" />                          
         <menuitem action="GRAYA" />                          
         <menuitem action="LIGHTBLUEA" />                          
         <menuitem action="LIGHTGREENA" />                          
         <menuitem action="MAGENTAA" />
         <menuitem action="ORANGEA" />
         <menuitem action="YELLOWA" />
         <menuitem action="WHITEA" />
       </menu> 
       <menu action="PENOPTA"> 
         <menuitem action="PENVERYFINEA" />     
         <menuitem action="PENFINEA" />                          
         <menuitem action="PENMEDIUMA" />                          
         <menuitem action="PENTHICKA" />                          
         <menuitem action="PENVERYTHICKA" />                          
       </menu>
       <menuitem action="ERASROPTA" />                        
       <menuitem action="HILTROPTA" />                        
       <menuitem action="TXTFNTA" />                        
       <separator />
       <menuitem action="DEFPENA" />                        
       <menuitem action="DEFERSRA" />    
       <menuitem action="DEFHILTRA" />                        
       <menuitem action="DEFTXTA" />
       <menuitem action="SETDEFOPTA" />                        
    </menu>
    <menu action="OMA">
       <menuitem action="UXINPUTA" />
       <menuitem action="DCRDCOREA" />                        
       <menuitem action="ERSRTIPA" />                        
       <menuitem action="PRESSRSENSA" />                        
       <menuitem action="PGHILTA" />                        
       <menuitem action="MLTPGVWA" />                        
       <menuitem action="MLTPGA" />                        
       <menuitem action="BTN2MAPA" />                        
       <menuitem action="BTN3MAPA" />                        
       <separator />
       <menuitem action="ANTIALIASBMPA" />                        
       <menuitem action="PRGRSBKGA" />                        
       <menuitem action="PRNTPPRULEA" />                        
       <menuitem action="LFTHNDSCRBRA" />                        
       <menuitem action="SHRTNMENUA" />   
       <separator />
       <menuitem action="AUTOSAVEPREFA" />                        
       <menuitem action="SAVEPREFA" />
    </menu>
    <menu action="HMA">
       <menuitem action="ABOUTA" />
    </menu>
  </menubar>
  <toolbar name="toolbar1" > 
    <toolitem action="SAVEA" />
    <toolitem action="NEWA" />
    <toolitem action="OPENA" />
    <separator />
    <toolitem action="CUTA" />
    <toolitem action="COPYA" />
    <toolitem action="PASTEA" />
    <separator /> 
    <toolitem action="UNDOA" /> 
    <toolitem action="REDOA" /> 
    <separator /> 
    <toolitem action="FSTPAGEA" />
    <toolitem action="PRVPAGEA" />
    <toolitem action="NXTPAGEA" />
    <toolitem action="LSTPAGEA" />
    <separator />
    <toolitem action="ZMOUTA" />      
    <toolitem action="NRMSIZEA" />                                        
    <toolitem action="ZMINA" />
    <toolitem action="PGWDTHA" />                          
    <toolitem action="SETZMA" />
    <toolitem action="FSCRA" />
  </toolbar>
  <toolbar name="toolbar2" > 
    <toolitem action="PENA"        />
    <toolitem action="ERASERA"     />                     
    <toolitem action="HIGHLTA"     />                     
    <toolitem action="TEXTA"       />                     
    <toolitem action="SHPRECA"     />   
    <toolitem action="RULERA"      />                     
    <separator />
    <toolitem action="SELREGNA"    />    
    <toolitem action="SELRECTA"    />                     
    <toolitem action="VERTSPA"     />                     
    <toolitem action="HANDA"       />                     
    <separator />     
    <toolitem action="DEFAULTA"    />    
    <toolitem action="DEFPENA"     />                     
    <separator />                     
    <toolitem action="PENFINEA"       />    
    <toolitem action="PENMEDIUMA"       />                     
    <toolitem action="PENTHICKA"       />   
    <separator />
    <toolitem action="BLACKA"      />                     
    <toolitem action="BLUEA"       />                     
    <toolitem action="REDA"        />                     
    <toolitem action="GREENA"      />                     
    <toolitem action="GRAYA"       />                     
    <toolitem action="LIGHTBLUEA"  />                      
    <toolitem action="LIGHTGREENA" />                     
    <toolitem action="MAGENTAA"    />                     
    <toolitem action="ORANGEA"     />                     
    <toolitem action="YELLOWA"     />                     
    <toolitem action="WHITEA"      />                     
  </toolbar>
</ui>
|]

iconList = [ ("fullscreen.png" , "myfullscreen")
           , ("pencil.png"     , "mypen")
           , ("eraser.png"     , "myeraser")
           , ("highlighter.png", "myhighlighter") 
           , ("text-tool.png"  , "mytext")
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

viewmods :: [RadioActionEntry] 
viewmods = [ RadioActionEntry "CONTA" "Continuous" Nothing Nothing Nothing 0
           , RadioActionEntry "ONEPAGEA" "One Page" Nothing Nothing Nothing 1
           ]
           
pointmods :: [RadioActionEntry] 
pointmods = [ RadioActionEntry "PENVERYFINEA" "Very fine" Nothing Nothing Nothing 0
            , RadioActionEntry "PENFINEA" "Fine" (Just "mythin") Nothing Nothing 1
            , RadioActionEntry "PENMEDIUMA" "Medium" (Just "mymedium") Nothing Nothing 2
            , RadioActionEntry "PENTHICKA" "Thick" (Just "mythick") Nothing Nothing 3 
            , RadioActionEntry "PENVERYTHICKA" "Very Thick" Nothing Nothing Nothing 4                
            ]            

penmods :: [RadioActionEntry] 
penmods = [ RadioActionEntry "PENA"    "Pen"         (Just "mypen")         Nothing Nothing 0
          , RadioActionEntry "ERASERA" "Eraser"      (Just "myeraser")      Nothing Nothing 1
          , RadioActionEntry "HIGHLTA" "Highlighter" (Just "myhighlighter") Nothing Nothing 2
          , RadioActionEntry "TEXTA"   "Text"        (Just "mytext")        Nothing Nothing 3 
          ]            

colormods = [ RadioActionEntry "BLACKA"      "Black"      (Just "myblack")      Nothing Nothing 0
            , RadioActionEntry "BLUEA"       "Blue"       (Just "myblue")       Nothing Nothing 1
            , RadioActionEntry "REDA"        "Red"        (Just "myred")        Nothing Nothing 2
            , RadioActionEntry "GREENA"      "Green"      (Just "mygreen")      Nothing Nothing 3
            , RadioActionEntry "GRAYA"       "Gray"       (Just "mygray")       Nothing Nothing 4
            , RadioActionEntry "LIGHTBLUEA"  "Lightblue"  (Just "mylightblue")  Nothing Nothing 5     
            , RadioActionEntry "LIGHTGREENA" "Lightgreen" (Just "mylightgreen") Nothing Nothing 6
            , RadioActionEntry "MAGENTAA"    "Magenta"    (Just "mymagenta")    Nothing Nothing 7
            , RadioActionEntry "ORANGEA"     "Orange"     (Just "myorange")     Nothing Nothing 8
            , RadioActionEntry "YELLOWA"     "Yellow"     (Just "myyellow")     Nothing Nothing 9
            , RadioActionEntry "WHITEA"      "White"      (Just "mywhite")      Nothing Nothing 10
            ]

iconResourceAdd :: IconFactory -> FilePath -> (FilePath, StockId) 
                   -> IO ()
iconResourceAdd iconfac resdir (fp,stid) = do 
  -- myimage <- imageNewFromFile (resdir </> fp)
  -- myIconSet <- iconSetNewFromPixbuf =<< imageGetPixbuf myimage 
  myIconSource <- iconSourceNew 
  iconSourceSetFilename myIconSource (resdir </> fp)
  --iconSourceSetPixbuf myIconSource =<< imageGetPixbuf myimage
  iconSourceSetSize myIconSource IconSizeLargeToolbar
  myIconSet <- iconSetNew 
  iconSetAddSource myIconSet myIconSource 
  iconFactoryAdd iconfac stid myIconSet



getMenuUI :: IORef (Await MyEvent (Iteratee MyEvent XournalStateIO ()))
             -> IORef XournalState 
             -> IO UIManager
getMenuUI tref sref = do 
  let actionNewAndRegister :: String -> String 
                           -> Maybe String -> Maybe StockId
                           -> Maybe MyEvent 
                           -> IO Action
      actionNewAndRegister name label tooltip stockId myevent = do 
        a <- actionNew name label tooltip stockId 
        case myevent of 
          Nothing -> return a 
          Just ev -> do 
            a `on` actionActivated $ do 
              bouncecallback tref sref ev
            return a


  -- icons   
  myiconfac <- iconFactoryNew 
  iconFactoryAddDefault myiconfac 
  
  resDir <- getDataDir >>= return . (</> "resource") 
  mapM_ (iconResourceAdd myiconfac resDir) iconList 
  
  fma     <- actionNewAndRegister "FMA"   "File" Nothing Nothing Nothing
  ema     <- actionNewAndRegister "EMA"   "Edit" Nothing Nothing Nothing
  vma     <- actionNewAndRegister "VMA"   "View" Nothing Nothing Nothing
  jma     <- actionNewAndRegister "JMA"   "Journal" Nothing Nothing Nothing
  tma     <- actionNewAndRegister "TMA"   "Tools" Nothing Nothing Nothing
  oma     <- actionNewAndRegister "OMA"   "Options" Nothing Nothing Nothing
  hma     <- actionNewAndRegister "HMA"   "Help" Nothing Nothing Nothing
  
  -- file menu
  newa    <- actionNewAndRegister "NEWA"  "New" (Just "Just a Stub") (Just stockNew) (Just MenuNew)
  annpdfa <- actionNewAndRegister "ANNPDFA" "Annotate PDf" (Just "Just a Stub") Nothing (Just MenuAnnotatePDF)
  opena   <- actionNewAndRegister "OPENA" "Open" (Just "Just a Stub") (Just stockOpen) (Just MenuOpen)
  savea   <- actionNewAndRegister "SAVEA" "Save" (Just "Just a Stub") (Just stockSave) (Just MenuSave)
  saveasa <- actionNewAndRegister "SAVEASA" "Save As" (Just "Just a Stub") (Just stockSaveAs) (Just MenuSaveAs)
  recenta <- actionNewAndRegister "RECENTA" "Recent Document" (Just "Just a Stub") Nothing (Just MenuRecentDocument)
  printa  <- actionNewAndRegister "PRINTA" "Print" (Just "Just a Stub") Nothing (Just MenuPrint)
  exporta <- actionNewAndRegister "EXPORTA" "Export" (Just "Just a Stub") Nothing (Just MenuExport)
  quita   <- actionNewAndRegister "QUITA" "Quit" (Just "Just a Stub") (Just stockQuit) (Just MenuQuit)
  
  -- edit menu
  undoa   <- actionNewAndRegister "UNDOA"   "Undo" (Just "Just a Stub") (Just stockUndo) (Just MenuUndo)
  redoa   <- actionNewAndRegister "REDOA"   "Redo" (Just "Just a Stub") (Just stockRedo) (Just MenuRedo)
  cuta    <- actionNewAndRegister "CUTA"    "Cut" (Just "Just a Stub")  (Just stockCut) (Just MenuCut)
  copya   <- actionNewAndRegister "COPYA"   "Copy" (Just "Just a Stub") (Just stockCopy) (Just MenuCopy)
  pastea  <- actionNewAndRegister "PASTEA"  "Paste" (Just "Just a Stub") (Just stockPaste) (Just MenuPaste)
  deletea <- actionNewAndRegister "DELETEA" "Delete" (Just "Just a Stub") (Just stockDelete) (Just MenuDelete)
  
  -- view menu
  fscra     <- actionNewAndRegister "FSCRA"     "Full Screen" (Just "Just a Stub") (Just "myfullscreen") (Just MenuFullScreen)
  zooma     <- actionNewAndRegister "ZOOMA"     "Zoom" (Just "Just a Stub") Nothing (Just MenuZoom)
  zmina     <- actionNewAndRegister "ZMINA"     "Zoom In" (Just "Zoom In") (Just stockZoomIn) (Just MenuZoomIn)
  zmouta    <- actionNewAndRegister "ZMOUTA"    "Zoom Out" (Just "Zoom Out") (Just stockZoomOut) (Just MenuZoomOut)
  nrmsizea  <- actionNewAndRegister "NRMSIZEA"  "Normal Size" (Just "Normal Size") (Just stockZoom100) (Just MenuNormalSize)
  pgwdtha   <- actionNewAndRegister "PGWDTHA" "Page Width" (Just "Page Width") (Just stockZoomFit) (Just MenuPageWidth)
  setzma    <- actionNewAndRegister "SETZMA"  "Set Zoom" (Just "Set Zoom") (Just stockFind) (Just MenuSetZoom)
  fstpagea  <- actionNewAndRegister "FSTPAGEA"  "First Page" (Just "Just a Stub") (Just stockGotoFirst) (Just MenuFirstPage)
  prvpagea  <- actionNewAndRegister "PRVPAGEA"  "Previous Page" (Just "Just a Stub") (Just stockGoBack) (Just MenuPreviousPage)
  nxtpagea  <- actionNewAndRegister "NXTPAGEA"  "Next Page" (Just "Just a Stub") (Just stockGoForward) (Just MenuNextPage)
  lstpagea  <- actionNewAndRegister "LSTPAGEA"  "Last Page" (Just "Just a Stub") (Just stockGotoLast) (Just MenuLastPage)
  shwlayera <- actionNewAndRegister "SHWLAYERA" "Show Layer" (Just "Just a Stub") Nothing (Just MenuShowLayer)
  hidlayera <- actionNewAndRegister "HIDLAYERA" "Hide Layer" (Just "Just a Stub") Nothing (Just MenuHideLayer)
  
  -- journal menu 
  newpgba <- actionNewAndRegister "NEWPGBA" "New Page Before" (Just "Just a Stub") Nothing (Just MenuNewPageBefore)
  newpgaa <- actionNewAndRegister "NEWPGAA" "New Page After"  (Just "Just a Stub") Nothing (Just MenuNewPageAfter)
  newpgea <- actionNewAndRegister "NEWPGEA" "New Page At End" (Just "Just a Stub") Nothing (Just MenuNewPageAtEnd)
  delpga  <- actionNewAndRegister "DELPGA"  "Delete Page"     (Just "Just a Stub") Nothing (Just MenuDeletePage)
  newlyra <- actionNewAndRegister "NEWLYRA" "New Layer"       (Just "Just a Stub") Nothing (Just MenuNewLayer)
  dellyra <- actionNewAndRegister "DELLYRA" "Delete Layer"    (Just "Just a Stub") Nothing (Just MenuDeleteLayer)
  ppsizea <- actionNewAndRegister "PPSIZEA" "Paper Size"      (Just "Just a Stub") Nothing (Just MenuPaperSize)
  ppclra  <- actionNewAndRegister "PPCLRA"  "Paper Color"     (Just "Just a Stub") Nothing (Just MenuPaperColor)
  ppstya  <- actionNewAndRegister "PPSTYA"  "Paper Style"     (Just "Just a Stub") Nothing (Just MenuPaperStyle)
  apallpga<- actionNewAndRegister "APALLPGA" "Apply To All Pages" (Just "Just a Stub") Nothing (Just MenuApplyToAllPages)
  ldbkga  <- actionNewAndRegister "LDBKGA"  "Load Background" (Just "Just a Stub") Nothing (Just MenuLoadBackground)
  bkgscrshta <- actionNewAndRegister "BKGSCRSHTA" "Background Screenshot" (Just "Just a Stub") Nothing (Just MenuBackgroundScreenshot)
  defppa  <- actionNewAndRegister "DEFPPA"  "Default Paper" (Just "Just a Stub") Nothing (Just MenuDefaultPaper)
  setdefppa <- actionNewAndRegister "SETDEFPPA" "Set As Default" (Just "Just a Stub") Nothing (Just MenuSetAsDefaultPaper)
  
  -- tools menu
  shpreca   <- actionNewAndRegister "SHPRECA" "Shape Recognizer" (Just "Just a Stub") (Just "myshapes") (Just MenuShapeRecognizer)
  rulera    <- actionNewAndRegister "RULERA" "Ruler" (Just "Just a Stub") (Just "myruler") (Just MenuRuler)
  selregna  <- actionNewAndRegister "SELREGNA" "Select Region" (Just "Just a Stub") (Just "mylasso") (Just MenuSelectRegion)
  selrecta  <- actionNewAndRegister "SELRECTA" "Select Rectangle" (Just "Just a Stub") (Just "myrectselect") (Just MenuSelectRectangle)
  vertspa   <- actionNewAndRegister "VERTSPA" "Vertical Space" (Just "Just a Stub") (Just "mystretch") (Just MenuVerticalSpace)
  handa     <- actionNewAndRegister "HANDA" "Hand Tool" (Just "Just a Stub") (Just "myhand") (Just MenuHandTool)
  clra      <- actionNewAndRegister "CLRA" "Color" (Just "Just a Stub") Nothing Nothing
  penopta   <- actionNewAndRegister "PENOPTA" "Pen Options" (Just "Just a Stub") Nothing (Just MenuPenOptions)
  erasropta <- actionNewAndRegister "ERASROPTA" "Eraser Options" (Just "Just a Stub") Nothing (Just MenuEraserOptions)
  hiltropta <- actionNewAndRegister "HILTROPTA" "Highlighter Options" (Just "Just a Stub") Nothing (Just MenuHighlighterOptions)
  txtfnta   <- actionNewAndRegister "TXTFNTA" "Text Font" (Just "Just a Stub") Nothing (Just MenuTextFont)
  defpena   <- actionNewAndRegister "DEFPENA" "Default Pen" (Just "Just a Stub") (Just "mydefaultpen") (Just MenuDefaultPen)
  defersra  <- actionNewAndRegister "DEFERSRA" "Default Eraser" (Just "Just a Stub") Nothing (Just MenuDefaultEraser)
  defhiltra <- actionNewAndRegister "DEFHILTRA" "Default Highlighter" (Just "Just a Stub") Nothing (Just MenuDefaultHighlighter)
  deftxta   <- actionNewAndRegister "DEFTXTA" "Default Text" (Just "Just a Stub") Nothing (Just MenuDefaultText)
  setdefopta <- actionNewAndRegister "SETDEFOPTA" "Set As Default" (Just "Just a Stub") Nothing (Just MenuSetAsDefaultOption)
  
  -- options menu 
  uxinputa <- actionNewAndRegister "UXINPUTA" "Use XInput" (Just "Just a Stub") Nothing (Just MenuUseXInput)
  dcrdcorea <- actionNewAndRegister "DCRDCOREA" "Discard Core Events" (Just "Just a Stub") Nothing (Just MenuDiscardCoreEvents)
  ersrtipa <- actionNewAndRegister "ERSRTIPA" "Eraser Tip" (Just "Just a Stub") Nothing (Just MenuEraserTip)
  pressrsensa <- actionNewAndRegister "PRESSRSENSA" "Pressure Sensitivity" (Just "Just a Stub") Nothing (Just MenuPressureSensitivity)
  pghilta <- actionNewAndRegister "PGHILTA" "Page Highlight" (Just "Just a Stub") Nothing (Just MenuPageHighlight)
  mltpgvwa <- actionNewAndRegister "MLTPGVWA" "Multiple Page View" (Just "Just a Stub") Nothing (Just MenuMultiplePageView) 
  mltpga <- actionNewAndRegister "MLTPGA" "Multiple Pages" (Just "Just a Stub") Nothing (Just MenuMultiplePages)
  btn2mapa <- actionNewAndRegister "BTN2MAPA" "Button 2 Mapping" (Just "Just a Stub") Nothing (Just MenuButton2Mapping)
  btn3mapa <- actionNewAndRegister "BTN3MAPA" "Button 3 Mapping" (Just "Just a Stub") Nothing (Just MenuButton3Mapping)
  antialiasbmpa <- actionNewAndRegister "ANTIALIASBMPA" "Antialiased Bitmaps" (Just "Just a Stub") Nothing (Just MenuAntialiasedBitmaps)
  prgrsbkga <- actionNewAndRegister "PRGRSBKGA" "Progressive Backgrounds" (Just "Just a Stub") Nothing (Just MenuProgressiveBackgrounds)
  prntpprulea <- actionNewAndRegister "PRNTPPRULEA" "Print Paper Ruling" (Just "Just a Stub") Nothing (Just MenuPrintPaperRuling)
  lfthndscrbra <- actionNewAndRegister "LFTHNDSCRBRA" "Left-Handed Scrollbar" (Just "Just a Stub") Nothing (Just MenuLeftHandedScrollbar)
  shrtnmenua <- actionNewAndRegister "SHRTNMENUA" "Shorten Menus" (Just "Just a Stub") Nothing (Just MenuShortenMenus)
  autosaveprefa <- actionNewAndRegister "AUTOSAVEPREFA" "Auto-Save Preferences" (Just "Just a Stub") Nothing (Just MenuAutoSavePreferences)
  saveprefa <- actionNewAndRegister "SAVEPREFA" "Save Preferences" (Just "Just a Stub") Nothing (Just MenuSavePreferences)
  
  -- help menu 
  abouta <- actionNewAndRegister "ABOUTA" "About" (Just "Just a Stub") Nothing (Just MenuAbout)

  -- others
  defaulta <- actionNewAndRegister "DEFAULTA" "Default" (Just "Default") (Just "mydefault") (Just MenuDefault)
  
  agr <- actionGroupNew "AGR"
  mapM_ (actionGroupAddAction agr) 
        [fma,ema,vma,jma,tma,oma,hma]
  mapM_ (\act -> actionGroupAddActionWithAccel agr act Nothing)   
        [ newa, annpdfa, opena, savea, saveasa, recenta, printa, exporta, quita
        , undoa, redoa, cuta, copya, pastea, deletea
        , fscra, zooma, zmina, zmouta, nrmsizea, pgwdtha, setzma
        , fstpagea, prvpagea, nxtpagea, lstpagea, shwlayera, hidlayera
        , newpgba, newpgaa, newpgea, delpga, newlyra, dellyra, ppsizea, ppclra
        , ppstya, apallpga, ldbkga, bkgscrshta, defppa, setdefppa
        , shpreca, rulera, selregna, selrecta, vertspa, handa, clra, penopta
        , erasropta, hiltropta, txtfnta, defpena, defersra, defhiltra, deftxta
        , setdefopta
        , uxinputa, dcrdcorea, ersrtipa, pressrsensa, pghilta, mltpgvwa
        , mltpga, btn2mapa, btn3mapa, antialiasbmpa, prgrsbkga, prntpprulea 
        , lfthndscrbra, shrtnmenua, autosaveprefa, saveprefa 
        , abouta 
        , defaulta         
        ] 
  actionGroupAddRadioActions agr viewmods 0 (\_ -> return ())
  actionGroupAddRadioActions agr pointmods 0 (assignPoint sref)
  actionGroupAddRadioActions agr penmods   0 (assignPenMode sref)
  actionGroupAddRadioActions agr colormods 0 (assignColor sref)
  ui <- uiManagerNew 
  uiManagerAddUiFromString ui uiDecl 
  uiManagerInsertActionGroup ui agr 0 
  return ui   

assignPenMode :: IORef XournalState -> RadioAction -> IO ()
assignPenMode sref a = do 
    v <- radioActionGetCurrentValue a
    let t = int2PenType v
    st <- readIORef sref 
    let pm = penMode st 
        pmNew = pm { pm_pentype = t }
        stNew = st { penMode = pmNew } 
    print $ pm
    writeIORef sref stNew 

assignColor :: IORef XournalState -> RadioAction -> IO () 
assignColor sref a = do 
    v <- radioActionGetCurrentValue a
    let c = int2Color v
    st <- readIORef sref 
    let pm = penMode st 
        pmNew = pm { pm_pencolor = c }
        stNew = st { penMode = pmNew } 
    print $ pm
    writeIORef sref stNew 

assignPoint :: IORef XournalState -> RadioAction -> IO () 
assignPoint sref a = do 
    v <- radioActionGetCurrentValue a
    let w = int2Point v
    st <- readIORef sref 
    let pm = penMode st 
        pmNew = pm { pm_penwidth = w }
        stNew = st { penMode = pmNew } 
    print $ pm
    writeIORef sref stNew 


int2PenType :: Int -> PenType 
int2PenType 0 = PenWork
int2PenType 1 = EraserWork
int2PenType 2 = HighlighterWork
int2PenType 3 = PenWork -- TextWork 

int2Point :: Int -> Double 
int2Point 0 = predefined_veryfine 
int2Point 1 = predefined_fine
int2Point 2 = predefined_medium
int2Point 3 = predefined_thick
int2Point 4 = predefined_verythick


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
