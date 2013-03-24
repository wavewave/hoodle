{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.GUI 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.GUI where

import           Control.Exception
import           Control.Lens (view)
import           Control.Monad
import           Control.Monad.Trans 
import qualified Data.IntMap as M
import           Data.IORef
import           Data.Maybe
import           Graphics.UI.Gtk hiding (get,set)
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
-- from this package
import           Hoodle.Accessor 
import           Hoodle.Config 
import           Hoodle.Coroutine
import           Hoodle.Coroutine.Callback
import           Hoodle.Device
import           Hoodle.ModelAction.Window
import           Hoodle.Script.Hook
import           Hoodle.Type.Canvas
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState 
--
import           Prelude hiding (catch)

-- |
startGUI :: Maybe FilePath -> Maybe Hook -> IO () 
startGUI mfname mhook = do 
  initGUI
  window <- windowNew   
  windowSetDefaultSize window 800 400
  cfg <- loadConfigFile   
  devlst <- initDevice cfg 
  maxundo <- getMaxUndo cfg >>= 
               \mmax -> maybe (return 50) (return . id) mmax
  xinputbool <- getXInputConfig cfg 
  (tref,st0,ui,vbox) <- initCoroutine devlst window mfname mhook maxundo  xinputbool
  setTitleFromFileName st0
  -- need for refactoring
  setToggleUIForFlag "UXINPUTA" (settings.doesUseXInput) st0 
  setToggleUIForFlag "POPMENUA" (settings.doesUsePopUpMenu) st0 
  setToggleUIForFlag "EBDIMGA" (settings.doesEmbedImage) st0 
  setToggleUIForFlag "EBDPDFA" (settings.doesEmbedPDF) st0
  -- 
  let canvases = map (getDrawAreaFromBox) . M.elems . getCanvasInfoMap $ st0
  if xinputbool
      then mapM_ (flip widgetSetExtensionEvents [ExtensionEventsAll]) canvases
      else mapM_ (flip widgetSetExtensionEvents [ExtensionEventsNone]) canvases
  -- 
  menubar <- uiManagerGetWidget ui "/ui/menubar" 
             >>= maybe (error "GUI.hs:no menubar") return 
  toolbar1 <- uiManagerGetWidget ui "/ui/toolbar1" 
              >>= maybe (error "GUI.hs:no toolbar1") return 
  toolbar2 <- uiManagerGetWidget ui "/ui/toolbar2"
              >>= maybe (error "GUI.hs:no toolbar2") return 
  -- 
  ebox <- eventBoxNew
  label <- labelNew (Just "drag me")
  containerAdd ebox label 
  
  dragSourceSet ebox [Button1] [ActionCopy]
  dragSourceSetIconStock ebox stockIndex
  dragSourceAddTextTargets ebox
  ebox `on` dragBegin $ \_dc -> do 
      liftIO $ putStrLn "dragging"
  ebox `on` dragDataGet $ \_dc _iid _ts -> do 
      -- very dirty solution but.. 
      minfo <- liftIO $ do 
        ref <- newIORef (Nothing :: Maybe String)
        view callBack st0 (GetHoodleFileInfo ref) 
        readIORef ref
      maybe (return ()) (selectionDataSetText >=> const (return ())) minfo
  --
  hbox <- hBoxNew False 0 
  boxPackStart hbox toolbar1 PackGrow 0
  boxPackStart hbox ebox PackNatural 0
  containerAdd window vbox
  boxPackStart vbox menubar PackNatural 0 
  boxPackStart vbox hbox PackNatural 0
  boxPackStart vbox toolbar2 PackNatural 0  
  boxPackEnd vbox (view rootWindow st0) PackGrow 0 
  window `on` deleteEvent $ do
    liftIO $ eventHandler tref (Menu MenuQuit)
    return True
  widgetShowAll window
  -- 
  let mainaction = do eventHandler tref Initialized     
                      mainGUI 
  mainaction `catch` \(_e :: SomeException) -> do 
    homepath <- getEnv "HOME"
    let dir = homepath </> ".hoodle.d"
    createDirectoryIfMissing False dir
    outh <- openFile (dir </> "error.log") WriteMode 
    hPutStrLn outh "error occured"
    hClose outh 
  return ()


