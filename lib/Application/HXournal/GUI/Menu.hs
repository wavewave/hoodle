{-# LANGUAGE QuasiQuotes #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.GUI.Menu 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.GUI.Menu where

import Application.HXournal.Util.Verbatim
import Application.HXournal.Coroutine.Callback
import Application.HXournal.Type
import Application.HXournal.Type.Clipboard
import Application.HXournal.Accessor
import Control.Monad.Coroutine.SuspensionFunctors
import Data.IORef
import Data.Maybe
import Control.Category
import Data.Label
import Prelude hiding ((.),id)
import Graphics.UI.Gtk hiding (set,get)
import qualified Graphics.UI.Gtk as Gtk (set)
import System.FilePath
import Data.Xournal.Predefined 
import Paths_hxournal

-- | 

justMenu :: MenuEvent -> Maybe MyEvent
justMenu = Just . Menu 

-- |

uiDeclTest :: String 
uiDeclTest = [verbatim|<ui> 
  <menubar>
    <menu action="VMA">
       <menuitem action="CONTA" />
       <menuitem action="ONEPAGEA" />
       <separator />
       <menuitem action="FSCRA" />
       <separator />
    </menu>
  </menubar>
</ui>|]

-- | 

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
       <separator />
       <menuitem action="NETCOPYA" />
       <menuitem action="NETPASTEA" />
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
         <menuitem action="PGHEIGHTA" />
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
       <separator />
       <menuitem action="HSPLITA" />
       <menuitem action="VSPLITA" />
       <menuitem action="DELCVSA" />
    </menu>
    <menu action="JMA">
       <menuitem action="NEWPGBA" />
       <menuitem action="NEWPGAA" />
       <menuitem action="NEWPGEA" />
       <menuitem action="DELPGA" />       
       <separator />
       <menuitem action="NEWLYRA" />
       <menuitem action="NEXTLAYERA" />
       <menuitem action="PREVLAYERA" />
       <menuitem action="GOTOLAYERA" />
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
         <menuitem action="PENULTRATHICKA" />     
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
       <menuitem action="RELAUNCHA" />
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
    <separator />     
    <toolitem action="DEFAULTA"    />    
    <toolitem action="DEFPENA"     />                     
    <toolitem action="SHPRECA"     />   
    <toolitem action="RULERA"      />                     
    <separator />
    <toolitem action="SELREGNA"    />    
    <toolitem action="SELRECTA"    />                     
    <toolitem action="VERTSPA"     />                     
    <toolitem action="HANDA"       />                     
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

iconList :: [ (String,String) ]
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

-- | 

viewmods :: [RadioActionEntry] 
viewmods = [ RadioActionEntry "CONTA" "Continuous" Nothing Nothing Nothing 0
           , RadioActionEntry "ONEPAGEA" "One Page" Nothing Nothing Nothing 1
           ]
           
-- | 

pointmods :: [RadioActionEntry] 
pointmods = [ RadioActionEntry "PENVERYFINEA" "Very fine" Nothing Nothing Nothing 0
            , RadioActionEntry "PENFINEA" "Fine" (Just "mythin") Nothing Nothing 1
            
            , RadioActionEntry "PENTHICKA" "Thick" (Just "mythick") Nothing Nothing 3 
            , RadioActionEntry "PENVERYTHICKA" "Very Thick" Nothing Nothing Nothing 4 
            , RadioActionEntry "PENULTRATHICKA" "Ultra Thick" Nothing Nothing Nothing 5   
            , RadioActionEntry "PENMEDIUMA" "Medium" (Just "mymedium") Nothing Nothing 2              
            ]            

-- | 

penmods :: [RadioActionEntry] 
penmods = [ RadioActionEntry "PENA"    "Pen"         (Just "mypen")         Nothing Nothing 0 
          , RadioActionEntry "ERASERA" "Eraser"      (Just "myeraser")      Nothing Nothing 1
          , RadioActionEntry "HIGHLTA" "Highlighter" (Just "myhighlighter") Nothing Nothing 2
          , RadioActionEntry "TEXTA"   "Text"        (Just "mytext")        Nothing Nothing 3 
          , RadioActionEntry "SELREGNA" "Select Region"     (Just "mylasso")        Nothing Nothing 4
          , RadioActionEntry "SELRECTA" "Select Rectangle" (Just "myrectselect")        Nothing Nothing 5
          , RadioActionEntry "VERTSPA" "Vertical Space"    (Just "mystretch")        Nothing Nothing 6
          , RadioActionEntry "HANDA"   "Hand Tool"         (Just "myhand")        Nothing Nothing 7
          ]            

-- | 

colormods :: [RadioActionEntry]
colormods = [ RadioActionEntry "BLUEA"       "Blue"       (Just "myblue")       Nothing Nothing 1
            , RadioActionEntry "REDA"        "Red"        (Just "myred")        Nothing Nothing 2
            , RadioActionEntry "GREENA"      "Green"      (Just "mygreen")      Nothing Nothing 3
            , RadioActionEntry "GRAYA"       "Gray"       (Just "mygray")       Nothing Nothing 4
            , RadioActionEntry "LIGHTBLUEA"  "Lightblue"  (Just "mylightblue")  Nothing Nothing 5     
            , RadioActionEntry "LIGHTGREENA" "Lightgreen" (Just "mylightgreen") Nothing Nothing 6
            , RadioActionEntry "MAGENTAA"    "Magenta"    (Just "mymagenta")    Nothing Nothing 7
            , RadioActionEntry "ORANGEA"     "Orange"     (Just "myorange")     Nothing Nothing 8
            , RadioActionEntry "YELLOWA"     "Yellow"     (Just "myyellow")     Nothing Nothing 9
            , RadioActionEntry "WHITEA"      "White"      (Just "mywhite")      Nothing Nothing 10
            , RadioActionEntry "BLACKA"      "Black"      (Just "myblack")      Nothing Nothing 0              
            ]

-- |

iconResourceAdd :: IconFactory -> FilePath -> (FilePath, StockId) 
                   -> IO ()
iconResourceAdd iconfac resdir (fp,stid) = do 
  myIconSource <- iconSourceNew 
  iconSourceSetFilename myIconSource (resdir </> fp)
  iconSourceSetSize myIconSource IconSizeLargeToolbar
  myIconSourceSmall <- iconSourceNew 
  iconSourceSetFilename myIconSourceSmall (resdir </> fp)
  iconSourceSetSize myIconSource IconSizeMenu
  myIconSet <- iconSetNew 
  iconSetAddSource myIconSet myIconSource 
  iconSetAddSource myIconSet myIconSourceSmall
  iconFactoryAdd iconfac stid myIconSet

-- | 

actionNewAndRegisterRef :: IORef (Await MyEvent (Iteratee MyEvent XournalStateIO ()))
                           -> IORef HXournalState
                           -> String -> String 
                           -> Maybe String -> Maybe StockId
                           -> Maybe MyEvent 
                           -> IO Action
actionNewAndRegisterRef tref sref name label tooltip stockId myevent = do 
    a <- actionNew name label tooltip stockId 
    case myevent of 
      Nothing -> return a 
      Just ev -> do 
        a `on` actionActivated $ do 
          bouncecallback tref sref ev
        return a

-- | 

getMenuUI :: IORef (Await MyEvent (Iteratee MyEvent XournalStateIO ()))
             -> IORef HXournalState 
             -> IO UIManager
getMenuUI tref sref = do 
  let actionNewAndRegister = actionNewAndRegisterRef tref sref 
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
  newa    <- actionNewAndRegister "NEWA"  "New" (Just "Just a Stub") (Just stockNew) (justMenu MenuNew)
  annpdfa <- actionNewAndRegister "ANNPDFA" "Annotate PDF" (Just "Just a Stub") Nothing (justMenu MenuAnnotatePDF)
  opena   <- actionNewAndRegister "OPENA" "Open" (Just "Just a Stub") (Just stockOpen) (justMenu MenuOpen)
  savea   <- actionNewAndRegister "SAVEA" "Save" (Just "Just a Stub") (Just stockSave) (justMenu MenuSave)
  saveasa <- actionNewAndRegister "SAVEASA" "Save As" (Just "Just a Stub") (Just stockSaveAs) (justMenu MenuSaveAs)
  recenta <- actionNewAndRegister "RECENTA" "Recent Document" (Just "Just a Stub") Nothing (justMenu MenuRecentDocument)
  printa  <- actionNewAndRegister "PRINTA" "Print" (Just "Just a Stub") Nothing (justMenu MenuPrint)
  exporta <- actionNewAndRegister "EXPORTA" "Export" (Just "Just a Stub") Nothing (justMenu MenuExport)
  quita   <- actionNewAndRegister "QUITA" "Quit" (Just "Just a Stub") (Just stockQuit) (justMenu MenuQuit)
  
  -- edit menu
  undoa   <- actionNewAndRegister "UNDOA"   "Undo" (Just "Just a Stub") (Just stockUndo) (justMenu MenuUndo)
  redoa   <- actionNewAndRegister "REDOA"   "Redo" (Just "Just a Stub") (Just stockRedo) (justMenu MenuRedo)
  cuta    <- actionNewAndRegister "CUTA"    "Cut" (Just "Just a Stub")  (Just stockCut) (justMenu MenuCut)
  copya   <- actionNewAndRegister "COPYA"   "Copy" (Just "Just a Stub") (Just stockCopy) (justMenu MenuCopy)
  pastea  <- actionNewAndRegister "PASTEA"  "Paste" (Just "Just a Stub") (Just stockPaste) (justMenu MenuPaste)
  deletea <- actionNewAndRegister "DELETEA" "Delete" (Just "Just a Stub") (Just stockDelete) (justMenu MenuDelete)
  -- netcopya <- actionNewAndRegister "NETCOPYA" "Copy to NetworkClipboard" (Just "Just a Stub") Nothing (justMenu MenuNetCopy)
  -- netpastea <- actionNewAndRegister "NETPASTEA" "Paste from NetworkClipboard" (Just "Just a Stub") Nothing (justMenu MenuNetPaste)

  -- view menu
  fscra     <- actionNewAndRegister "FSCRA"     "Full Screen" (Just "Just a Stub") (Just "myfullscreen") (justMenu MenuFullScreen)
  zooma     <- actionNewAndRegister "ZOOMA"     "Zoom" (Just "Just a Stub") Nothing Nothing -- (justMenu MenuZoom)
  zmina     <- actionNewAndRegister "ZMINA"     "Zoom In" (Just "Zoom In") (Just stockZoomIn) (justMenu MenuZoomIn)
  zmouta    <- actionNewAndRegister "ZMOUTA"    "Zoom Out" (Just "Zoom Out") (Just stockZoomOut) (justMenu MenuZoomOut)
  nrmsizea  <- actionNewAndRegister "NRMSIZEA"  "Normal Size" (Just "Normal Size") (Just stockZoom100) (justMenu MenuNormalSize)
  pgwdtha   <- actionNewAndRegister "PGWDTHA" "Page Width" (Just "Page Width") (Just stockZoomFit) (justMenu MenuPageWidth)
  pgheighta <- actionNewAndRegister "PGHEIGHTA" "Page Height" (Just "Page Height") Nothing (justMenu MenuPageHeight)
  setzma    <- actionNewAndRegister "SETZMA"  "Set Zoom" (Just "Set Zoom") (Just stockFind) (justMenu MenuSetZoom)
  fstpagea  <- actionNewAndRegister "FSTPAGEA"  "First Page" (Just "Just a Stub") (Just stockGotoFirst) (justMenu MenuFirstPage)
  prvpagea  <- actionNewAndRegister "PRVPAGEA"  "Previous Page" (Just "Just a Stub") (Just stockGoBack) (justMenu MenuPreviousPage)
  nxtpagea  <- actionNewAndRegister "NXTPAGEA"  "Next Page" (Just "Just a Stub") (Just stockGoForward) (justMenu MenuNextPage)
  lstpagea  <- actionNewAndRegister "LSTPAGEA"  "Last Page" (Just "Just a Stub") (Just stockGotoLast) (justMenu MenuLastPage)
  shwlayera <- actionNewAndRegister "SHWLAYERA" "Show Layer" (Just "Just a Stub") Nothing (justMenu MenuShowLayer)
  hidlayera <- actionNewAndRegister "HIDLAYERA" "Hide Layer" (Just "Just a Stub") Nothing (justMenu MenuHideLayer)
  hsplita <- actionNewAndRegister "HSPLITA" "Horizontal Split" (Just "horizontal split") Nothing (justMenu MenuHSplit)
  vsplita <- actionNewAndRegister "VSPLITA" "Vertical Split" (Just "vertical split") Nothing (justMenu MenuVSplit)
  delcvsa <- actionNewAndRegister "DELCVSA" "Delete Current Canvas" (Just "delete current canvas") Nothing (justMenu MenuDelCanvas)

  -- journal menu 
  newpgba <- actionNewAndRegister "NEWPGBA" "New Page Before" (Just "Just a Stub") Nothing (justMenu MenuNewPageBefore)
  newpgaa <- actionNewAndRegister "NEWPGAA" "New Page After"  (Just "Just a Stub") Nothing (justMenu MenuNewPageAfter)
  newpgea <- actionNewAndRegister "NEWPGEA" "New Page At End" (Just "Just a Stub") Nothing (justMenu MenuNewPageAtEnd)
  delpga  <- actionNewAndRegister "DELPGA"  "Delete Page"     (Just "Just a Stub") Nothing (justMenu MenuDeletePage)
  newlyra <- actionNewAndRegister "NEWLYRA" "New Layer"       (Just "Just a Stub") Nothing (justMenu MenuNewLayer)
  nextlayera <- actionNewAndRegister "NEXTLAYERA" "Next Layer" (Just "Just a Stub") Nothing (justMenu MenuNextLayer)
  prevlayera <- actionNewAndRegister "PREVLAYERA" "Prev Layer" (Just "Just a Stub") Nothing (justMenu MenuPrevLayer)
  gotolayera <- actionNewAndRegister "GOTOLAYERA" "Goto Layer" (Just "Just a Stub") Nothing (justMenu MenuGotoLayer)
  dellyra <- actionNewAndRegister "DELLYRA" "Delete Layer"    (Just "Just a Stub") Nothing (justMenu MenuDeleteLayer)
  ppsizea <- actionNewAndRegister "PPSIZEA" "Paper Size"      (Just "Just a Stub") Nothing (justMenu MenuPaperSize)
  ppclra  <- actionNewAndRegister "PPCLRA"  "Paper Color"     (Just "Just a Stub") Nothing (justMenu MenuPaperColor)
  ppstya  <- actionNewAndRegister "PPSTYA"  "Paper Style"     (Just "Just a Stub") Nothing (justMenu MenuPaperStyle)
  apallpga<- actionNewAndRegister "APALLPGA" "Apply To All Pages" (Just "Just a Stub") Nothing (justMenu MenuApplyToAllPages)
  ldbkga  <- actionNewAndRegister "LDBKGA"  "Load Background" (Just "Just a Stub") Nothing (justMenu MenuLoadBackground)
  bkgscrshta <- actionNewAndRegister "BKGSCRSHTA" "Background Screenshot" (Just "Just a Stub") Nothing (justMenu MenuBackgroundScreenshot)
  defppa  <- actionNewAndRegister "DEFPPA"  "Default Paper" (Just "Just a Stub") Nothing (justMenu MenuDefaultPaper)
  setdefppa <- actionNewAndRegister "SETDEFPPA" "Set As Default" (Just "Just a Stub") Nothing (justMenu MenuSetAsDefaultPaper)
  
  -- tools menu
  shpreca   <- actionNewAndRegister "SHPRECA" "Shape Recognizer" (Just "Just a Stub") (Just "myshapes") (justMenu MenuShapeRecognizer)
  rulera    <- actionNewAndRegister "RULERA" "Ruler" (Just "Just a Stub") (Just "myruler") (justMenu MenuRuler)
  -- selregna  <- actionNewAndRegister "SELREGNA" "Select Region" (Just "Just a Stub") (Just "mylasso") (justMenu MenuSelectRegion)
  -- selrecta  <- actionNewAndRegister "SELRECTA" "Select Rectangle" (Just "Just a Stub") (Just "myrectselect") (justMenu MenuSelectRectangle)
  -- vertspa   <- actionNewAndRegister "VERTSPA" "Vertical Space" (Just "Just a Stub") (Just "mystretch") (justMenu MenuVerticalSpace)
  -- handa     <- actionNewAndRegister "HANDA" "Hand Tool" (Just "Just a Stub") (Just "myhand") (justMenu MenuHandTool) 
  clra      <- actionNewAndRegister "CLRA" "Color" (Just "Just a Stub") Nothing Nothing
  penopta   <- actionNewAndRegister "PENOPTA" "Pen Options" (Just "Just a Stub") Nothing (justMenu MenuPenOptions)
  erasropta <- actionNewAndRegister "ERASROPTA" "Eraser Options" (Just "Just a Stub") Nothing (justMenu MenuEraserOptions)
  hiltropta <- actionNewAndRegister "HILTROPTA" "Highlighter Options" (Just "Just a Stub") Nothing (justMenu MenuHighlighterOptions)
  txtfnta   <- actionNewAndRegister "TXTFNTA" "Text Font" (Just "Just a Stub") Nothing (justMenu MenuTextFont)
  defpena   <- actionNewAndRegister "DEFPENA" "Default Pen" (Just "Just a Stub") (Just "mydefaultpen") (justMenu MenuDefaultPen)
  defersra  <- actionNewAndRegister "DEFERSRA" "Default Eraser" (Just "Just a Stub") Nothing (justMenu MenuDefaultEraser)
  defhiltra <- actionNewAndRegister "DEFHILTRA" "Default Highlighter" (Just "Just a Stub") Nothing (justMenu MenuDefaultHighlighter)
  deftxta   <- actionNewAndRegister "DEFTXTA" "Default Text" (Just "Just a Stub") Nothing (justMenu MenuDefaultText)
  setdefopta <- actionNewAndRegister "SETDEFOPTA" "Set As Default" (Just "Just a Stub") Nothing (justMenu MenuSetAsDefaultOption)
  relauncha <- actionNewAndRegister "RELAUNCHA" "Relaunch Application" (Just "Just a Stub") Nothing (justMenu MenuRelaunch)
    
  -- options menu 
  uxinputa <- toggleActionNew "UXINPUTA" "Use XInput" (Just "Just a Stub") Nothing 
  uxinputa `on` actionToggled $ do 
    bouncecallback tref sref (Menu MenuUseXInput)
--               AndRegister "UXINPUTA" "Use XInput" (Just "Just a Stub") Nothing (justMenu MenuUseXInput)
  dcrdcorea <- actionNewAndRegister "DCRDCOREA" "Discard Core Events" (Just "Just a Stub") Nothing (justMenu MenuDiscardCoreEvents)
  ersrtipa <- actionNewAndRegister "ERSRTIPA" "Eraser Tip" (Just "Just a Stub") Nothing (justMenu MenuEraserTip)
  pressrsensa <- toggleActionNew "PRESSRSENSA" "Pressure Sensitivity" (Just "Just a Stub") Nothing 
  pressrsensa `on` actionToggled $ do 
    bouncecallback tref sref (Menu MenuPressureSensitivity)
--               AndRegister "UXINPUTA" "Use XInput" (Just "Just a Stub") Nothing (justMenu MenuUseXInput)

  
  
  
  pghilta <- actionNewAndRegister "PGHILTA" "Page Highlight" (Just "Just a Stub") Nothing (justMenu MenuPageHighlight)
  mltpgvwa <- actionNewAndRegister "MLTPGVWA" "Multiple Page View" (Just "Just a Stub") Nothing (justMenu MenuMultiplePageView) 
  mltpga <- actionNewAndRegister "MLTPGA" "Multiple Pages" (Just "Just a Stub") Nothing (justMenu MenuMultiplePages)
  btn2mapa <- actionNewAndRegister "BTN2MAPA" "Button 2 Mapping" (Just "Just a Stub") Nothing (justMenu MenuButton2Mapping)
  btn3mapa <- actionNewAndRegister "BTN3MAPA" "Button 3 Mapping" (Just "Just a Stub") Nothing (justMenu MenuButton3Mapping)
  antialiasbmpa <- actionNewAndRegister "ANTIALIASBMPA" "Antialiased Bitmaps" (Just "Just a Stub") Nothing (justMenu MenuAntialiasedBitmaps)
  prgrsbkga <- actionNewAndRegister "PRGRSBKGA" "Progressive Backgrounds" (Just "Just a Stub") Nothing (justMenu MenuProgressiveBackgrounds)
  prntpprulea <- actionNewAndRegister "PRNTPPRULEA" "Print Paper Ruling" (Just "Just a Stub") Nothing (justMenu MenuPrintPaperRuling)
  lfthndscrbra <- actionNewAndRegister "LFTHNDSCRBRA" "Left-Handed Scrollbar" (Just "Just a Stub") Nothing (justMenu MenuLeftHandedScrollbar)
  shrtnmenua <- actionNewAndRegister "SHRTNMENUA" "Shorten Menus" (Just "Just a Stub") Nothing (justMenu MenuShortenMenus)
  autosaveprefa <- actionNewAndRegister "AUTOSAVEPREFA" "Auto-Save Preferences" (Just "Just a Stub") Nothing (justMenu MenuAutoSavePreferences)
  saveprefa <- actionNewAndRegister "SAVEPREFA" "Save Preferences" (Just "Just a Stub") Nothing (justMenu MenuSavePreferences)
  
  -- help menu 
  abouta <- actionNewAndRegister "ABOUTA" "About" (Just "Just a Stub") Nothing (justMenu MenuAbout)

  -- others
  defaulta <- actionNewAndRegister "DEFAULTA" "Default" (Just "Default") (Just "mydefault") (justMenu MenuDefault)
  
  agr <- actionGroupNew "AGR"
  mapM_ (actionGroupAddAction agr) 
        [fma,ema,vma,jma,tma,oma,hma]
  mapM_ (actionGroupAddAction agr)   
        [ undoa, redoa, cuta, copya, pastea, deletea ] 
  -- actionGroupAddActionWithAccel agr undoa (Just "<control>z")   
  mapM_ (\act -> actionGroupAddActionWithAccel agr act Nothing)   
        [ newa, annpdfa, opena, savea, saveasa, recenta, printa, exporta, quita
        {- , netcopya, netpastea -}
        , fscra, zooma, zmina, zmouta, nrmsizea, pgwdtha, pgheighta, setzma
        , fstpagea, prvpagea, nxtpagea, lstpagea, shwlayera, hidlayera
        , hsplita, vsplita, delcvsa
        , newpgba, newpgaa, newpgea, delpga, newlyra, nextlayera, prevlayera, gotolayera, dellyra, ppsizea, ppclra
        , ppstya, apallpga, ldbkga, bkgscrshta, defppa, setdefppa
        , shpreca, rulera, clra, penopta 
        , erasropta, hiltropta, txtfnta, defpena, defersra, defhiltra, deftxta
        , setdefopta, relauncha
        , dcrdcorea, ersrtipa, pghilta, mltpgvwa
        , mltpga, btn2mapa, btn3mapa, antialiasbmpa, prgrsbkga, prntpprulea 
        , lfthndscrbra, shrtnmenua, autosaveprefa, saveprefa 
        , abouta 
        , defaulta         
        ] 
    
  actionGroupAddAction agr uxinputa 
  actionGroupAddAction agr pressrsensa
  
  -- actionGroupAddRadioActions agr viewmods 0 (assignViewMode tref sref)
  actionGroupAddRadioActions agr viewmods 0 (const (return ()))
  
  
  actionGroupAddRadioActions agr pointmods 0 (assignPoint sref)
  actionGroupAddRadioActions agr penmods   0 (assignPenMode tref sref)
  actionGroupAddRadioActions agr colormods 0 (assignColor sref) 
 
  let disabledActions = 
        [ recenta, printa, exporta
        , cuta, copya, pastea, deletea
        , fscra,  setzma
        , shwlayera, hidlayera
        , newpgea, {- delpga, -} ppsizea, ppclra
        , ppstya, apallpga, ldbkga, bkgscrshta, defppa, setdefppa
        , shpreca, rulera 
        , erasropta, hiltropta, txtfnta, defpena, defersra, defhiltra, deftxta
        , setdefopta
        , dcrdcorea, ersrtipa, pghilta, mltpgvwa
        , mltpga, btn2mapa, btn3mapa, antialiasbmpa, prgrsbkga, prntpprulea 
        , lfthndscrbra, shrtnmenua, autosaveprefa, saveprefa 
        , abouta 
        , defaulta         
        ] 
      enabledActions = 
        [ opena, savea, saveasa, quita, fstpagea, prvpagea, nxtpagea, lstpagea
        , clra, penopta, zooma, nrmsizea, pgwdtha 
        ]
  --
  mapM_ (\x->actionSetSensitive x True) enabledActions  
  mapM_ (\x->actionSetSensitive x False) disabledActions
  --

  
  
  -- 
  -- radio actions
  --
  ui <- uiManagerNew 
  uiManagerAddUiFromString ui uiDecl
  uiManagerInsertActionGroup ui agr 0 
  -- Just ra1 <- actionGroupGetAction agr "ONEPAGEA"
  -- Gtk.set (castToRadioAction ra1) [radioActionCurrentValue := 1]  
  Just ra2 <- actionGroupGetAction agr "PENFINEA"
  Gtk.set (castToRadioAction ra2) [radioActionCurrentValue := 2]
  Just ra3 <- actionGroupGetAction agr "SELREGNA"
  actionSetSensitive ra3 True 
  Just ra4 <- actionGroupGetAction agr "VERTSPA"
  actionSetSensitive ra4 False
  Just ra5 <- actionGroupGetAction agr "HANDA"
  actionSetSensitive ra5 False
  Just ra6 <- actionGroupGetAction agr "CONTA"
  actionSetSensitive ra6 True
  Just toolbar1 <- uiManagerGetWidget ui "/ui/toolbar1"
  toolbarSetStyle (castToToolbar toolbar1) ToolbarIcons 
  Just toolbar2 <- uiManagerGetWidget ui "/ui/toolbar2"
  toolbarSetStyle (castToToolbar toolbar2) ToolbarIcons 
  return ui   

-- | 

assignViewMode :: IORef (Await MyEvent (Iteratee MyEvent XournalStateIO ()))
                 -> IORef HXournalState -> RadioAction -> IO ()
assignViewMode tref sref a = do 
    st <- readIORef sref 
    putStrLn "in assignmViewMode"
    printCanvasMode (getCurrentCanvasId st) (get currentCanvasInfo st)
    putStrLn "still in assignViewMode"
    viewModeToMyEvent a >>= bouncecallback tref sref
    
-- | 

assignPenMode :: IORef (Await MyEvent (Iteratee MyEvent XournalStateIO ()))
                 -> IORef HXournalState -> RadioAction -> IO ()
assignPenMode tref sref a = do 
    v <- radioActionGetCurrentValue a
    let t = int2PenType v
    st <- readIORef sref 
    case t of 
      Left pm -> do 
        let stNew = set (penType.penInfo) pm st 
        writeIORef sref stNew 
        bouncecallback tref sref ToViewAppendMode
      Right sm -> do 
        let stNew = set (selectType.selectInfo) sm st 
        writeIORef sref stNew 
        bouncecallback tref sref ToSelectMode
        
-- | 

assignColor :: IORef HXournalState -> RadioAction -> IO () 
assignColor sref a = do 
    v <- radioActionGetCurrentValue a
    let c = int2Color v
    st <- readIORef sref 
    let callback = get callBack st
    let stNew = set (penColor.currentTool.penInfo) c st 
    writeIORef sref stNew 
    callback (PenColorChanged c)

-- | 

assignPoint :: IORef HXournalState -> RadioAction -> IO () 
assignPoint sref a = do 
    v <- radioActionGetCurrentValue a
    st <- readIORef sref 
    let ptype = get (penType.penInfo) st
    let w = int2Point ptype v
    let stNew = set (penWidth.currentTool.penInfo) w st 
    let callback = get callBack st        
    writeIORef sref stNew 
    callback (PenWidthChanged w)

-- | 

int2PenType :: Int -> Either PenType SelectType 
int2PenType 0 = Left PenWork
int2PenType 1 = Left EraserWork
int2PenType 2 = Left HighlighterWork
int2PenType 3 = Left TextWork 
int2PenType 4 = Right SelectRegionWork
int2PenType 5 = Right SelectRectangleWork
int2PenType 6 = Right SelectVerticalSpaceWork
int2PenType 7 = Right SelectHandToolWork
int2PenType _ = error "No such pentype"

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
int2Point TextWork 0 = predefined_veryfine
int2Point TextWork 1 = predefined_fine
int2Point TextWork 2 = predefined_medium
int2Point TextWork 3 = predefined_thick
int2Point TextWork 4 = predefined_verythick
int2Point TextWork 5 = predefined_ultrathick
int2Point _ _ = error "No such point"

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
