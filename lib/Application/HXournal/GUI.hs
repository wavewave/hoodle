{-# LANGUAGE ScopedTypeVariables #-}

module Application.HXournal.GUI where

import Application.HXournal.Type 
import Application.HXournal.Coroutine
import Application.HXournal.Device
import Application.HXournal.GUI.Menu

import Graphics.UI.Gtk hiding (get)
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.State
import Control.Monad.Coroutine

import Application.HXournal.Iteratee
import Application.HXournal.Draw
import Application.HXournal.Device
import Data.IORef

import Text.Xournal.Type
import Text.Xournal.Parse

import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.C
import Foreign.Ptr

startGUI :: FilePath -> IO () 
startGUI fname = do 
  initGUI
  dev <- initDevice 
  print dev
  putStrLn uiDecl 
  
  canvas <- drawingAreaNew
  scrwin <- scrolledWindowNew Nothing Nothing 
  containerAdd scrwin canvas
  
  
  xojcontent <- read_xojgz fname 
  let st = emptyXournalState { xoj = xojcontent, darea = canvas, device = dev} 
  (r,st') <- runStateT (resume iter) st
  sref <- newIORef st'

  tref <- case r of 
            Left aw -> do 
              newIORef aw 
            Right _ -> error "what?"

  writeIORef sref st' {callback = bouncecallback tref sref }
  
  window <- windowNew 
  -- hbox <- hBoxNew False 0 
  vbox <- vBoxNew False 0 
  ui <- getMenuUI tref sref  
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
  boxPackEnd vbox scrwin PackGrow 0 
  canvas `on` sizeRequest $ return (Requisition 480 400)

  cursorDot <- cursorNew BlankCursor
  canvas `on` enterNotifyEvent $ tryEvent $ do 
    win <- liftIO $ widgetGetDrawWindow canvas
    liftIO $ drawWindowSetCursor win (Just cursorDot)
    return ()

  canvas `on` exposeEvent $ tryEvent $ do 
    liftIO $ bouncecallback tref sref UpdateCanvas 
  
  canvas `on` buttonPressEvent $ tryEvent $ do 
    st <- liftIO (readIORef sref)
    let callbk = callback st
        dev = device st 
    p <- getPointer dev
    liftIO (callbk (PenDown p))
 
  canvas `on` buttonReleaseEvent $ tryEvent $ do 
    st <- liftIO (readIORef sref)
    let callbk = callback st
        dev = device st 
    p <- getPointer dev
    liftIO (callbk (PenUp p))
    

  widgetAddEvents canvas [PointerMotionMask,Button1MotionMask]
  widgetSetExtensionEvents canvas [ExtensionEventsAll]

  
  onDestroy window mainQuit
  
  widgetShowAll window
  mainGUI 
  return ()
  
