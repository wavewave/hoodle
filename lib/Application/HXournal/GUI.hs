{-# LANGUAGE ScopedTypeVariables #-}

module Application.HXournal.GUI where

import Application.HXournal.Type 
import Application.HXournal.Coroutine
import Application.HXournal.Device
import Application.HXournal.Util 
import Application.HXournal.GUI.Menu

import Graphics.UI.Gtk hiding (get,set)
import Control.Monad.Coroutine.SuspensionFunctors
import qualified Control.Monad.State as St
import Control.Monad.IO.Class
import Control.Monad.Coroutine

import Application.HXournal.Iteratee
import Application.HXournal.Draw
import Application.HXournal.Device
import Data.IORef

import Control.Category
import Data.Label
import Prelude hiding ((.),id)

import Text.Xournal.Type
import qualified Text.Xournal.Parse as P

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
  
  hadj <- adjustmentNew 0 0 500 100 200 200 
  vadj <- adjustmentNew 0 0 500 100 200 200 
  scrolledWindowSetHAdjustment scrwin hadj 
  scrolledWindowSetVAdjustment scrwin vadj 
  
  xojcontent <- P.read_xournal fname 
  let Dim w h = page_dim . (!! 0) .  xoj_pages $ xojcontent
  let st = set xournal xojcontent  
           . set drawArea canvas
           . set deviceList dev
           . set viewInfo (ViewInfo OnePage Original (0,0) (w,h))
           . set horizAdjustment hadj 
           . set vertAdjustment vadj 
           $ emptyHXournalState
  (r,st') <- St.runStateT (resume guiProcess) st
  sref <- newIORef st'

  tref <- case r of 
            Left aw -> do 
              newIORef aw 
            Right _ -> error "what?"

  writeIORef sref . set callBack (bouncecallback tref sref) $ st'
  
  window <- windowNew 
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
  
  afterValueChanged hadj $ do 
    v <- adjustmentGetValue hadj 
    bouncecallback tref sref (HScrollBarMoved v)
    
  afterValueChanged vadj $ do 
    v <- adjustmentGetValue vadj     
    bouncecallback tref sref (VScrollBarMoved v)

  canvas `on` sizeRequest $ return (Requisition 480 400)
  canvas `on` configureEvent $ tryEvent $ do 
    (w,h) <- eventSize 
    liftIO $ bouncecallback tref sref 
                            (CanvasConfigure (fromIntegral w) (fromIntegral h))
  cursorDot <- cursorNew BlankCursor
  canvas `on` enterNotifyEvent $ tryEvent $ do 
    win <- liftIO $ widgetGetDrawWindow canvas
    liftIO $ drawWindowSetCursor win (Just cursorDot)
    return ()

  canvas `on` exposeEvent $ tryEvent $ do 
    liftIO $ bouncecallback tref sref UpdateCanvas 
  
  canvas `on` buttonPressEvent $ tryEvent $ do 
    st <- liftIO (readIORef sref)
    let callbk = get callBack st
        dev = get deviceList st 
    p <- getPointer dev
    liftIO (callbk (PenDown p))
 
  canvas `on` buttonReleaseEvent $ tryEvent $ do 
    st <- liftIO (readIORef sref)
    let callbk = get callBack st
        dev = get deviceList st 
    p <- getPointer dev
    liftIO (callbk (PenUp p))
    
  widgetAddEvents canvas [PointerMotionMask,Button1MotionMask]
  widgetSetExtensionEvents canvas [ExtensionEventsAll]
  
  onDestroy window mainQuit

  widgetShowAll window
  
  -- initialized
  bouncecallback tref sref Initialized     
  
  mainGUI 
  return ()
  
