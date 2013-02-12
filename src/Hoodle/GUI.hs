{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.GUI 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.GUI where

import           Control.Category
import           Control.Exception
import           Control.Lens
import           Control.Monad.Trans 
import qualified Data.IntMap as M
import           Data.Maybe
-- import           Data.Time
import           Graphics.UI.Gtk hiding (get,set)
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
-- 
-- import           Control.Monad.Trans.Crtn.EventHandler 
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
import           Prelude hiding ((.),id,catch)

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
  {- handlebox1 <- handleBoxNew 
  containerAdd handlebox1 toolbar1 
  handlebox2 <- handleBoxNew 
  containerAdd handlebox2 toolbar2
  widgetSetSizeRequest handlebox1 400 (-1)
  widgetSetSizeRequest handlebox2 400 (-1)
  -- windowSetTransientFor handlebox1 window
  -- windowSetTransientFor handlebox2 window -}
  --
  containerAdd window vbox
  -- containerAdd window handlebox1
  -- containerAdd window handlebox2 
  boxPackStart vbox menubar PackNatural 0 
  boxPackStart vbox toolbar1 PackNatural 0
  boxPackStart vbox toolbar2 PackNatural 0  
  -- boxPackStart vbox handlebox1 PackNatural 0 
  -- boxPackStart vbox handlebox2 PackNatural 0 
  boxPackEnd vbox (view rootWindow st0) PackGrow 0 
  -- cursorDot <- cursorNew BlankCursor  
  window `on` deleteEvent $ do
    liftIO $ eventHandler tref (Menu MenuQuit)
    return True
  widgetShowAll window
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




  {- let menubar = case maybeMenubar of 
                  Just x  -> x 
                  Nothing -> error "cannot get menubar from string" 
  
  maybeToolbar1 <- uiManagerGetWidget ui "/ui/toolbar1"
  let toolbar1 = case maybeToolbar1 of 
                   Just x  -> x     
                   Nothing -> error "cannot get toolbar from string"
  maybeToolbar2 <- uiManagerGetWidget ui "/ui/toolbar2"
  let toolbar2 = case maybeToolbar2 of 
                   Just x  -> x     
                   Nothing -> error "cannot get toolbar from string"  -}
