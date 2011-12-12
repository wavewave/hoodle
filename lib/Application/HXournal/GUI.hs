{-# LANGUAGE ScopedTypeVariables #-}

module Application.HXournal.GUI where

import Application.HXournal.Type.XournalState 
import Application.HXournal.Type.Event

import Application.HXournal.Coroutine
import Application.HXournal.Device
import Application.HXournal.Iteratee
import Application.HXournal.GUI.Menu
import Application.HXournal.ModelAction.File 
import Application.HXournal.ModelAction.Window

import Graphics.UI.Gtk hiding (get,set)

import Control.Applicative 

import Data.IORef

import Control.Category
import Data.Label
import Prelude hiding ((.),id)


startGUI :: Maybe FilePath -> IO () 
startGUI mfname = do 
  initGUI
  window <- windowNew   
  
  devlst <- initDevice 
  (tref,sref) <- initCoroutine devlst window
  st0 <- readIORef sref 
  st1 <- getFileContent mfname st0
  writeIORef sref st1
  (winCvsArea, wconf) <- constructFrame 
                         <$> get frameState 
                         <*> get canvasInfoMap $ st1
  
  setTitleFromFileName st1
  vbox <- vBoxNew False 0 
  
  let st2 = set frameState wconf 
            . set rootWindow winCvsArea 
            . set rootContainer (castToBox vbox) $ st1 
  writeIORef sref st2
  ui <- getMenuUI tref sref  
  
  let st3 = set gtkUIManager ui st2 
  writeIORef sref st3 
  
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
  onDestroy window mainQuit
  widgetShowAll window
  
  -- initialized
  bouncecallback tref sref Initialized     
  mainGUI 
  return ()
  
