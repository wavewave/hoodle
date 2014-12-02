{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.GUI 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : GPL-3
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
import           Graphics.UI.Gtk hiding (get,set,Settings)
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
    initGUI
    window <- windowNew   
    windowSetDefaultSize window 800 400
    cfg <- loadConfigFile   
    devlst <- initDevice cfg 
    maxundo <- getMaxUndo cfg >>= maybe (return 50) (return . id)
    xinputbool <- getXInputConfig cfg 
    (usepz,uselyr) <- getWidgetConfig cfg 
    (tref,st0,ui,vbox) <- initCoroutine devlst window mhook maxundo (xinputbool,usepz,uselyr) 
    setTitleFromFileName st0
    -- need for refactoring


    mapM_ (\(x,y :: Simple Lens Settings Bool) -> lensSetToggleUIForFlag x (settings.y) st0 )
      [ ("UXINPUTA", doesUseXInput) 
      , ("HANDA"   , doesUseTouch)
      , ("POPMENUA", doesUsePopUpMenu)
      , ("EBDIMGA" , doesEmbedImage)
      , ("EBDPDFA" , doesEmbedPDF)
      ] 
    setToggleUIForFlag "TOGGLENETSRCA" False st0
    -- 
    let canvases = map (getDrawAreaFromBox) . M.elems . view (unitHoodles.currentUnit.cvsInfoMap) $ st0
    if xinputbool
        then mapM_ (flip widgetSetExtensionEvents [ExtensionEventsAll]) canvases
        else mapM_ (flip widgetSetExtensionEvents [ExtensionEventsNone]) canvases
    --
    outerLayout ui vbox st0 
    window `on` deleteEvent $ do
      liftIO $ eventHandler tref (UsrEv (Menu MenuQuit))
      return True
    widgetShowAll window

    forkIO $ clock (eventHandler tref)

    let mainaction = do eventHandler tref (UsrEv (Initialized mfname))
                        mainGUI 
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
    postGUIAsync (evhandler (SysEv ClockUpdateEvent))



outerLayout :: UIManager -> VBox -> HoodleState -> IO ()
outerLayout ui vbox xst = do 
    let notebook = view rootNotebook xst
        mstatusbar = view statusBar xst
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
    -- ebox `on` dragBegin $ \_dc -> do 
    --   liftIO $ putStrLn "dragging"
    ebox `on` dragDataGet $ \_dc _iid _ts -> do 
      -- very dirty solution but.. 
      minfo <- liftIO $ do 
        ref <- newIORef (Nothing :: Maybe String)
        view callBack xst (UsrEv (GetHoodleFileInfo ref))
        readIORef ref
      traverse selectionDataSetText minfo >> return ()

    -- 
    hbox <- hBoxNew False 0 
    boxPackStart hbox toolbar1 PackGrow 0
    boxPackStart hbox ebox PackNatural 0
    boxPackStart vbox menubar PackNatural 0 
    boxPackStart vbox hbox PackNatural 0
    boxPackStart vbox toolbar2 PackNatural 0  
    forM_ mstatusbar $ \statusbar-> boxPackEnd vbox statusbar PackNatural 0
    --
    boxPackStart vbox notebook PackGrow 0 

