{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hoodle.GUI where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException (..), catch)
import Control.Lens (view)
import Control.Monad (forever, return, void, (>>), (>>=))
import Control.Monad.Trans (liftIO)
import Data.Foldable (forM_, traverse_)
import Data.IORef (newIORef, readIORef)
import Data.Maybe (Maybe (..), maybe)
import qualified Graphics.UI.Gtk as Gtk
import Hoodle.Accessor
  ( lensSetToggleUIForFlag,
    setToggleUIForFlag,
  )
import Hoodle.Config
  ( getMaxUndo,
    getPenConfig,
    getWidgetConfig,
    getXInputConfig,
    loadConfigFile,
  )
import Hoodle.Coroutine.Callback (eventHandler)
import Hoodle.Coroutine.Default (initCoroutine)
import Hoodle.Device (initDevice)
import Hoodle.ModelAction.Window (setTitleFromFileName)
import Hoodle.Script.Hook (Hook)
import Hoodle.Type.Event
  ( AllEvent (SysEv, UsrEv),
    MenuEvent (MenuQuit),
    SystemEvent (ClockUpdateEvent),
    UserEvent
      ( GetHoodleFileInfo,
        Initialized,
        Menu
      ),
  )
import Hoodle.Type.HoodleState
  ( HoodleState,
    callBack,
    doesEmbedImage,
    doesEmbedPDF,
    doesUsePopUpMenu,
    doesUseTouch,
    doesUseXInput,
    rootNotebook,
    settings,
    statusBar,
  )
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO
  ( FilePath,
    IO,
    IOMode (WriteMode),
    hClose,
    hPutStrLn,
    openFile,
  )
--
import Prelude (Bool (..), String, error, ($), (.))

-- |
startGUI :: Maybe FilePath -> Maybe Hook -> IO ()
startGUI mfname mhook = do
  _ <- Gtk.initGUI
  window <- Gtk.windowNew
  Gtk.windowSetDefaultSize window 800 400
  cfg <- loadConfigFile
  devlst <- initDevice cfg
  maxundo <- getMaxUndo cfg >>= maybe (return 50) return
  xinputbool <- getXInputConfig cfg
  varcsr <- getPenConfig cfg
  (usepz, uselyr) <- getWidgetConfig cfg
  (tref, st0, ui, vbox) <- initCoroutine devlst window mhook maxundo (xinputbool, usepz, uselyr, varcsr)
  setTitleFromFileName st0
  void $ lensSetToggleUIForFlag "UXINPUTA" (settings . doesUseXInput) st0
  void $ lensSetToggleUIForFlag "HANDA" (settings . doesUseTouch) st0
  void $ lensSetToggleUIForFlag "POPMENUA" (settings . doesUsePopUpMenu) st0
  void $ lensSetToggleUIForFlag "EBDIMGA" (settings . doesEmbedImage) st0
  void $ lensSetToggleUIForFlag "EBDPDFA" (settings . doesEmbedPDF) st0

  _ <- setToggleUIForFlag "TOGGLENETSRCA" False st0
  --
  outerLayout ui vbox st0
  _ <- window `Gtk.on` Gtk.deleteEvent $ do
    liftIO $ eventHandler tref (UsrEv (Menu MenuQuit))
    return True
  Gtk.widgetShowAll window

  _ <- forkIO $ clock (eventHandler tref)
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
  threadDelay 1_000_000
  Gtk.postGUIAsync (evhandler (SysEv ClockUpdateEvent))

outerLayout :: Gtk.UIManager -> Gtk.VBox -> HoodleState -> IO ()
outerLayout ui vbox xst = do
  let notebook = view rootNotebook xst
      mstatusbar = view statusBar xst
  menubar <-
    Gtk.uiManagerGetWidget ui "/ui/menubar"
      >>= maybe (error "GUI.hs:no menubar") return
  toolbar1 <-
    Gtk.uiManagerGetWidget ui "/ui/toolbar1"
      >>= maybe (error "GUI.hs:no toolbar1") return
  toolbar2 <-
    Gtk.uiManagerGetWidget ui "/ui/toolbar2"
      >>= maybe (error "GUI.hs:no toolbar2") return
  --
  ebox <- Gtk.eventBoxNew
  label <- Gtk.labelNew (Just "drag me")
  Gtk.containerAdd ebox label
  Gtk.dragSourceSet ebox [Gtk.Button1] [Gtk.ActionCopy]
  Gtk.dragSourceSetIconStock ebox Gtk.stockIndex
  Gtk.dragSourceAddTextTargets ebox
  _ <- ebox `Gtk.on` Gtk.dragDataGet $ \_dc _iid _ts -> do
    -- very dirty solution but..
    minfo <- liftIO $ do
      ref <- newIORef (Nothing :: Maybe String)
      view callBack xst (UsrEv (GetHoodleFileInfo ref))
      readIORef ref
    traverse_ Gtk.selectionDataSetText minfo

  --
  hbox <- Gtk.hBoxNew False 0
  Gtk.boxPackStart hbox toolbar1 Gtk.PackGrow 0
  Gtk.boxPackStart hbox ebox Gtk.PackNatural 0
  Gtk.boxPackStart vbox menubar Gtk.PackNatural 0
  Gtk.boxPackStart vbox hbox Gtk.PackNatural 0
  Gtk.boxPackStart vbox toolbar2 Gtk.PackNatural 0
  forM_ mstatusbar $ \statusbar -> Gtk.boxPackEnd vbox statusbar Gtk.PackNatural 0
  --
  Gtk.boxPackStart vbox notebook Gtk.PackGrow 0
