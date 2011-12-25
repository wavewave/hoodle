{-# LANGUAGE ScopedTypeVariables #-}

module Application.HXournal.GUI where

import Application.HXournal.Type.XournalState 
import Application.HXournal.Type.Event
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Undo 
import qualified Data.IntMap as M

import Application.HXournal.Coroutine.Callback

import Application.HXournal.Config 
import Application.HXournal.Device
import Application.HXournal.Coroutine
-- import Application.HXournal.GUI.Menu
import Application.HXournal.ModelAction.File 
import Application.HXournal.ModelAction.Window

import Graphics.UI.Gtk hiding (get,set)

import Control.Applicative 
import Control.Monad.Trans 

import Data.IORef
import Data.Maybe

import Control.Category
import Data.Label
import Prelude hiding ((.),id)


startGUI :: Maybe FilePath -> IO () 
startGUI mfname = do 
  initGUI
  window <- windowNew   

  cfg <- loadConfigFile   
  devlst <- initDevice cfg 
  (tref,sref) <- initCoroutine devlst window
  st0 <- readIORef sref 
  maxundo <- getMaxUndo cfg >>= 
               \mmax -> maybe (return 50) (return . id) mmax
  let st1 = set undoTable (emptyUndo maxundo) st0 
            
  -- let st1 = set gtkUIManager ui st0
  st2 <- getFileContent mfname st1
  let ui = get gtkUIManager st2 
  writeIORef sref st2
  (winCvsArea, wconf) <- constructFrame 
                         <$> get frameState 
                         <*> get canvasInfoMap $ st2
  
  setTitleFromFileName st2
  vbox <- vBoxNew False 0 
  
  
  let st3 = set frameState wconf 
            . set rootWindow winCvsArea 
            . set rootContainer (castToBox vbox) $ st2
  writeIORef sref st3
  
  xinputbool <- getXInputConfig cfg 
  agr <- uiManagerGetActionGroups ui >>= \x ->
           case x of 
             [] -> error "No action group? "
             y:_ -> return y 
  uxinputa <- actionGroupGetAction agr "UXINPUTA" >>= \(Just x) -> 
                return (castToToggleAction x) 
  toggleActionSetActive uxinputa xinputbool
  let canvases = map (get drawArea) . M.elems . get canvasInfoMap $ st3
  if xinputbool
      then mapM_ (flip widgetSetExtensionEvents [ExtensionEventsAll]) canvases
      else mapM_ (flip widgetSetExtensionEvents [ExtensionEventsNone]) canvases
  

  maybeMenubar <- uiManagerGetWidget ui "/ui/menubar"
  let menubar = case maybeMenubar of 
                  Just x  -> x 
                  Nothing -> error "cannot get menubar from string"
  
  maybeToolbar1 <- uiManagerGetWidget ui "/ui/toolbar1"
  let toolbar1 = case maybeToolbar1 of 
                   Just x  -> x     
                   Nothing -> error "cannot get toolbar from string"
  maybeToolbar2 <- uiManagerGetWidget ui "/ui/toolbar2"
  let toolbar2 = case maybeToolbar2 of 
                   Just x  -> x     
                   Nothing -> error "cannot get toolbar from string" 
  
  containerAdd window vbox
  
  boxPackStart vbox menubar PackNatural 0 
  boxPackStart vbox toolbar1 PackNatural 0
  boxPackStart vbox toolbar2 PackNatural 0 
  boxPackEnd vbox winCvsArea PackGrow 0 
  -- cursorDot <- cursorNew BlankCursor  
  window `on` deleteEvent $ do
    liftIO $ bouncecallback tref sref (Menu MenuQuit)
    return True
  widgetShowAll window
  
  -- initialized
  bouncecallback tref sref Initialized     
  mainGUI 
  return ()
  
