{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.GUI 
-- Copyright   : (c) 2011-2016 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.GUI where

import           Control.Concurrent (threadDelay, forkIO)
import           Control.Exception (SomeException(..),catch)
import           Control.Lens
import           Control.Monad hiding (mapM_,forM_)
import           Control.Monad.Trans 
import           Data.Foldable (mapM_,forM_)
import qualified Data.IntMap as M
import           Data.IORef
import           Data.Maybe
import qualified Graphics.UI.Gtk as Gtk
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
-- from this package
import           Hoodle.Accessor 
import           Hoodle.Config 
import           Hoodle.Coroutine.Callback
import           Hoodle.Coroutine.Default
import           Hoodle.Device
import           Hoodle.ModelAction.Window
import           Hoodle.Script.Hook
import           Hoodle.Type.Canvas
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState 
--
import           Prelude ((.),($),String,Bool(..),error,flip,id,map) 

-- |
startGUI :: Maybe FilePath -> Maybe Hook -> IO () 
startGUI mfname mhook = do 
    Gtk.initGUI
    window <- Gtk.windowNew   
    Gtk.windowSetDefaultSize window 800 400
    cfg <- loadConfigFile   
    devlst <- initDevice cfg 
    maxundo <- getMaxUndo cfg >>= maybe (return 50) (return . id)
    xinputbool <- getXInputConfig cfg 
    varcsr <- getPenConfig cfg
    (usepz,uselyr) <- getWidgetConfig cfg 
    (tref,st0,ui,vbox) <- initCoroutine devlst window mhook maxundo (xinputbool,usepz,uselyr,varcsr) 
    setTitleFromFileName st0
    -- need for refactoring
    {- 
    mapM_ (\(x,y :: Simple Lens Settings Bool ) -> lensSetToggleUIForFlag  x (settings.y) st0 )
      [ ("UXINPUTA", doesUseXInput) 
      , ("HANDA"   , doesUseTouch)
      , ("POPMENUA", doesUsePopUpMenu)
      , ("EBDIMGA" , doesEmbedImage)
      , ("EBDPDFA" , doesEmbedPDF)
      ]   -}
    lensSetToggleUIForFlag "UXINPUTA" (settings.doesUseXInput)    st0
    lensSetToggleUIForFlag "HANDA"    (settings.doesUseTouch)     st0
    lensSetToggleUIForFlag "POPMENUA" (settings.doesUsePopUpMenu) st0
    lensSetToggleUIForFlag "EBDIMGA"  (settings.doesEmbedImage)   st0
    lensSetToggleUIForFlag "EBDPDFA"  (settings.doesEmbedPDF)     st0
    
      
    setToggleUIForFlag "TOGGLENETSRCA" False st0
    -- 
    let canvases = map (getDrawAreaFromBox) . M.elems . view (unitHoodles.currentUnit.cvsInfoMap) $ st0
    outerLayout ui vbox st0 
    window `Gtk.on` Gtk.deleteEvent $ do
      liftIO $ eventHandler tref (UsrEv (Menu MenuQuit))
      return True
    Gtk.widgetShowAll window

    forkIO $ clock (eventHandler tref)
    let mainaction = eventHandler tref (UsrEv (Initialized mfname)) >> Gtk.mainGUI 
    mainaction `catch` \(_e :: SomeException) -> do 
      homepath <- getEnv "HOME"
      let dir = homepath </> ".hoodle.d"
      createDirectoryIfMissing False dir
      outh <- openFile (dir </> "error.log") WriteMode 
      hPutStrLn outh "error occured"
      hClose outh 
    return ()


clock :: (AllEvent -> IO ()) -> IO ()
clock evhandler = forever $ do 
    threadDelay 1000000
    Gtk.postGUIAsync (evhandler (SysEv ClockUpdateEvent))



outerLayout :: Gtk.UIManager -> Gtk.VBox -> HoodleState -> IO ()
outerLayout ui vbox xst = do 
    let notebook = view rootNotebook xst
        mstatusbar = view statusBar xst
    menubar <- Gtk.uiManagerGetWidget ui "/ui/menubar" 
               >>= maybe (error "GUI.hs:no menubar") return 
    toolbar1 <- Gtk.uiManagerGetWidget ui "/ui/toolbar1" 
                >>= maybe (error "GUI.hs:no toolbar1") return 
    toolbar2 <- Gtk.uiManagerGetWidget ui "/ui/toolbar2"
                >>= maybe (error "GUI.hs:no toolbar2") return 
    -- 
    ebox <- Gtk.eventBoxNew
    label <- Gtk.labelNew (Just "drag me")
    Gtk.containerAdd ebox label 
    Gtk.dragSourceSet ebox [Gtk.Button1] [Gtk.ActionCopy]
    Gtk.dragSourceSetIconStock ebox Gtk.stockIndex
    Gtk.dragSourceAddTextTargets ebox
    -- ebox `on` dragBegin $ \_dc -> do 
    --   liftIO $ putStrLn "dragging"
    ebox `Gtk.on` Gtk.dragDataGet $ \_dc _iid _ts -> do 
      -- very dirty solution but.. 
      minfo <- liftIO $ do 
        ref <- newIORef (Nothing :: Maybe String)
        view callBack xst (UsrEv (GetHoodleFileInfo ref))
        readIORef ref
      traverse Gtk.selectionDataSetText minfo >> return ()

    -- 
    hbox <- Gtk.hBoxNew False 0 
    Gtk.boxPackStart hbox toolbar1 Gtk.PackGrow 0
    Gtk.boxPackStart hbox ebox Gtk.PackNatural 0
    Gtk.boxPackStart vbox menubar Gtk.PackNatural 0 
    Gtk.boxPackStart vbox hbox Gtk.PackNatural 0
    Gtk.boxPackStart vbox toolbar2 Gtk.PackNatural 0  
    forM_ mstatusbar $ \statusbar-> Gtk.boxPackEnd vbox statusbar Gtk.PackNatural 0
    --
    Gtk.boxPackStart vbox notebook Gtk.PackGrow 0 

