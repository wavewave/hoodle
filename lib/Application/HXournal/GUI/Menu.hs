{-# LANGUAGE QuasiQuotes #-}

module Application.HXournal.GUI.Menu where

import Application.HXournal.Util.Verbatim
import Graphics.UI.Gtk

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
       <menuitem action="ZOOMA" />
       <separator />
       <menuitem action="FSTPAGEA" />
       <menuitem action="SNDPAGEA" />
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
       <menuitem action="CLRA" />
       <menuitem action="PENOPTA" />
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
</ui>
|]

getMenuUI :: IO UIManager
getMenuUI = do 
  fma     <- actionNew "FMA"   "File" Nothing Nothing 
  ema     <- actionNew "EMA"   "Edit" Nothing Nothing 
  vma     <- actionNew "VMA"   "View" Nothing Nothing 
  jma     <- actionNew "JMA"   "Journal" Nothing Nothing 
  tma     <- actionNew "TMA"   "Tools" Nothing Nothing 
  oma     <- actionNew "OMA"   "Options" Nothing Nothing
  hma     <- actionNew "HMA"   "Help" Nothing Nothing 
  
  -- file menu
  newa    <- actionNew "NEWA"  "New" (Just "Just a Stub") (Just stockNew)
  annpdfa <- actionNew "ANNPDFA" "Annotate PDf" (Just "Just a Stub") Nothing
  opena   <- actionNew "OPENA" "Open" (Just "Just a Stub") (Just stockOpen)
  savea   <- actionNew "SAVEA" "Save" (Just "Just a Stub") (Just stockSave)
  saveasa <- actionNew "SAVEASA" "Save As" (Just "Just a Stub") (Just stockSaveAs)
  recenta <- actionNew "RECENTA" "Recent Document" (Just "Just a Stub") Nothing
  printa  <- actionNew "PRINTA" "Print" (Just "Just a Stub") Nothing
  exporta <- actionNew "EXPORTA" "Export" (Just "Just a Stub") Nothing
  quita   <- actionNew "QUITA" "Quit" (Just "Just a Stub") (Just stockQuit)
  
  -- edit menu
  undoa   <- actionNew "UNDOA"   "Undo" (Just "Just a Stub") Nothing
  redoa   <- actionNew "REDOA"   "Redo" (Just "Just a Stub") Nothing
  cuta    <- actionNew "CUTA"    "Cut" (Just "Just a Stub") Nothing
  copya   <- actionNew "COPYA"   "Copy" (Just "Just a Stub") Nothing
  pastea  <- actionNew "PASTEA"  "Paste" (Just "Just a Stub") Nothing
  deletea <- actionNew "DELETEA" "Delete" (Just "Just a Stub") Nothing
  
  -- view menu
  -- conta     <- actionNew "CONTA"     "Continuous" (Just "Just a Stub") Nothing
  -- onepagea  <- actionNew "ONEPAGEA"  "One Page" (Just "Just a Stub") Nothing
  fscra     <- actionNew "FSCRA"     "Full Screen" (Just "Just a Stub") Nothing
  zooma     <- actionNew "ZOOMA"     "Zoom" (Just "Just a Stub") Nothing
  fstpagea  <- actionNew "FSTPAGEA"  "First Page" (Just "Just a Stub") Nothing
  sndpagea  <- actionNew "SNDPAGEA"  "Second Page" (Just "Just a Stub") Nothing
  nxtpagea  <- actionNew "NXTPAGEA"  "Next Page" (Just "Just a Stub") Nothing
  lstpagea  <- actionNew "LSTPAGEA"  "Last Page" (Just "Just a Stub") Nothing
  shwlayera <- actionNew "SHWLAYERA" "Show Layer" (Just "Just a Stub") Nothing
  hidlayera <- actionNew "HIDLAYERA" "Hide Layer" (Just "Just a Stub") Nothing
  
  -- journal menu 
  newpgba <- actionNew "NEWPGBA" "New Page Before" (Just "Just a Stub") Nothing
  newpgaa <- actionNew "NEWPGAA" "New Page After"  (Just "Just a Stub") Nothing
  newpgea <- actionNew "NEWPGEA" "New Page At End" (Just "Just a Stub") Nothing
  delpga  <- actionNew "DELPGA"  "Delete Page"     (Just "Just a Stub") Nothing
  newlyra <- actionNew "NEWLYRA" "New Layer"       (Just "Just a Stub") Nothing
  dellyra <- actionNew "DELLYRA" "Delete Layer"    (Just "Just a Stub") Nothing
  ppsizea <- actionNew "PPSIZEA" "Paper Size"      (Just "Just a Stub") Nothing
  ppclra  <- actionNew "PPCLRA"  "Paper Color"     (Just "Just a Stub") Nothing
  ppstya  <- actionNew "PPSTYA"  "Paper Style"     (Just "Just a Stub") Nothing
  apallpga<- actionNew "APALLPGA" "Apply To All Pages" (Just "Just a Stub") Nothing
  ldbkga  <- actionNew "LDBKGA"  "Load Background" (Just "Just a Stub") Nothing
  bkgscrshta <- actionNew "BKGSCRSHTA" "Background Screenshot" (Just "Just a Stub") Nothing
  defppa  <- actionNew "DEFPPA"  "Default Paper" (Just "Just a Stub") Nothing
  setdefppa <- actionNew "SETDEFPPA" "Set As Default" (Just "Just a Stub") Nothing
  
  -- tools menu
  shpreca   <- actionNew "SHPRECA" "Shape Recognizer" (Just "Just a Stub") Nothing
  rulera    <- actionNew "RULERA" "Ruler" (Just "Just a Stub") Nothing
  selregna  <- actionNew "SELREGNA" "Select Region" (Just "Just a Stub") Nothing
  selrecta  <- actionNew "SELRECTA" "Select Rectangle" (Just "Just a Stub") Nothing
  vertspa   <- actionNew "VERTSPA" "Vertical Space" (Just "Just a Stub") Nothing
  handa     <- actionNew "HANDA" "Hand Tool" (Just "Just a Stub") Nothing
  clra      <- actionNew "CLRA" "Color" (Just "Just a Stub") Nothing
  penopta   <- actionNew "PENOPTA" "Pen Options" (Just "Just a Stub") Nothing
  erasropta <- actionNew "ERASROPTA" "Eraser Options" (Just "Just a Stub") Nothing
  hiltropta <- actionNew "HILTROPTA" "Highlighter Options" (Just "Just a Stub") Nothing
  txtfnta   <- actionNew "TXTFNTA" "Text Font" (Just "Just a Stub") Nothing
  defpena   <- actionNew "DEFPENA" "Default Pen" (Just "Just a Stub") Nothing
  defersra  <- actionNew "DEFERSRA" "Default Eraser" (Just "Just a Stub") Nothing
  defhiltra <- actionNew "DEFHILTRA" "Default Highlighter" (Just "Just a Stub") Nothing
  deftxta   <- actionNew "DEFTXTA" "Default Text" (Just "Just a Stub") Nothing
  setdefopta <- actionNew "SETDEFOPTA" "Set As Default" (Just "Just a Stub") Nothing
  
  -- options menu 
  uxinputa <- actionNew "UXINPUTA" "Use XInput" (Just "Just a Stub") Nothing
  dcrdcorea <- actionNew "DCRDCOREA" "Discard Core Events" (Just "Just a Stub") Nothing
  ersrtipa <- actionNew "ERSRTIPA" "Eraser Tip" (Just "Just a Stub") Nothing
  pressrsensa <- actionNew "PRESSRSENSA" "Pressure Sensitivity" (Just "Just a Stub") Nothing
  pghilta <- actionNew "PGHILTA" "Page Highlight" (Just "Just a Stub") Nothing
  mltpgvwa <- actionNew "MLTPGVWA" "Multiple Page View" (Just "Just a Stub") Nothing
  mltpga <- actionNew "MLTPGA" "Multiple Pages" (Just "Just a Stub") Nothing
  btn2mapa <- actionNew "BTN2MAPA" "Button 2 Mapping" (Just "Just a Stub") Nothing
  btn3mapa <- actionNew "BTN3MAPA" "Button 3 Mapping" (Just "Just a Stub") Nothing
  antialiasbmpa <- actionNew "ANTIALIASBMPA" "Antialiased Bitmaps" (Just "Just a Stub") Nothing
  prgrsbkga <- actionNew "PRGRSBKGA" "Progressive Backgrounds" (Just "Just a Stub") Nothing
  prntpprulea <- actionNew "PRNTPPRULEA" "Print Paper Ruling" (Just "Just a Stub") Nothing
  lfthndscrbra <- actionNew "LFTHNDSCRBRA" "Left-Handed Scrollbar" (Just "Just a Stub") Nothing
  shrtnmenua <- actionNew "SHRTNMENUA" "Shorten Menus" (Just "Just a Stub") Nothing
  autosaveprefa <- actionNew "AUTOSAVEPREFA" "Auto-Save Preferences" (Just "Just a Stub") Nothing
  saveprefa <- actionNew "SAVEPREFA" "Save Preferences" (Just "Just a Stub") Nothing
  
  
  -- help menu 
  abouta <- actionNew "ABOUTA" "About" (Just "Just a Stub") Nothing 
  
  
  agr <- actionGroupNew "AGR"
  mapM_ (actionGroupAddAction agr) 
        [fma,ema,vma,jma,tma,oma,hma]
  mapM_ (\act -> actionGroupAddActionWithAccel agr act Nothing)   
        [ newa, annpdfa, opena, savea, saveasa, recenta, printa, exporta, quita
        , undoa, redoa, cuta, copya, pastea, deletea
        , fscra, zooma, fstpagea, sndpagea, nxtpagea, lstpagea, shwlayera, hidlayera
        , newpgba, newpgaa, newpgea, delpga, newlyra, dellyra, ppsizea, ppclra
        , ppstya, apallpga, ldbkga, bkgscrshta, defppa, setdefppa
        , shpreca, rulera, selregna, selrecta, vertspa, handa, clra, penopta
        , erasropta, hiltropta, txtfnta, defpena, defersra, defhiltra, deftxta
        , setdefopta
        , uxinputa, dcrdcorea, ersrtipa, pressrsensa, pghilta, mltpgvwa
        , mltpga, btn2mapa, btn3mapa, antialiasbmpa, prgrsbkga, prntpprulea 
        , lfthndscrbra, shrtnmenua, autosaveprefa, saveprefa 
        , abouta 
        
        ] 
  actionGroupAddRadioActions agr viewmods 0 (\_ -> return ())
  actionGroupAddRadioActions agr penmods 0 (\_ -> return ())  
  
  ui <- uiManagerNew 
  uiManagerAddUiFromString ui uiDecl 
  uiManagerInsertActionGroup ui agr 0 
  return ui   


viewmods :: [RadioActionEntry] 
viewmods = [ RadioActionEntry "CONTA" "Continuous" Nothing Nothing Nothing 0
           , RadioActionEntry "ONEPAGEA" "One Page" Nothing Nothing Nothing 1
           ]
           
penmods :: [RadioActionEntry] 
penmods = [ RadioActionEntry "PENA" "Pen" Nothing Nothing Nothing 0
          , RadioActionEntry "ERASERA" "Eraser" Nothing Nothing Nothing 1
          , RadioActionEntry "HIGHLTA" "Highlighter" Nothing Nothing Nothing 2
          , RadioActionEntry "TEXTA" "Text" Nothing Nothing Nothing 3 ]            