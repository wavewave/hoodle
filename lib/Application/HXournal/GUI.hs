{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.GUI 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

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
import Application.HXournal.ModelAction.Page
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
  -- ncconf <- getNetworkInfo  cfg
  
  let st1 = set undoTable (emptyUndo maxundo) st0 
            
  -- let st1 = set gtkUIManager ui st0
  putStrLn "before st2"
  st2 <- getFileContent mfname st1
  putStrLn "after st2"
  let ui = get gtkUIManager st2 
  writeIORef sref st2
  -- (st3, winCvsArea, wconf) <- constructFrame <*> get frameState $ st2
  let st3 = st2                          
  setTitleFromFileName st3
  vbox <- vBoxNew False 0 
  
  let st4 = set rootContainer (castToBox vbox) st3
  writeIORef sref st4
  xinputbool <- getXInputConfig cfg 
  agr <- uiManagerGetActionGroups ui >>= \x ->
           case x of 
             [] -> error "No action group? "
             y:_ -> return y 
  uxinputa <- actionGroupGetAction agr "UXINPUTA" >>= \(Just x) -> 
                return (castToToggleAction x) 
  toggleActionSetActive uxinputa xinputbool
  let canvases = map (getDrawAreaFromBox) . M.elems . get canvasInfoMap $ st4
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
  boxPackEnd vbox (get rootWindow st4) PackGrow 0 
  -- cursorDot <- cursorNew BlankCursor  
  window `on` deleteEvent $ do
    liftIO $ bouncecallback tref sref (Menu MenuQuit)
    return True
  putStrLn "before widget show all " 
  widgetShowAll window
  putStrLn "after widget show all " 
  
  -- initialized
  bouncecallback tref sref Initialized     
  mainGUI 
  return ()
  
