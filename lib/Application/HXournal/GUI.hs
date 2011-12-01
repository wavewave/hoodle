{-# LANGUAGE ScopedTypeVariables #-}

module Application.HXournal.GUI where

import Application.HXournal.Type 
import Application.HXournal.Type.Event
import Graphics.Xournal.Render.BBox
import Application.HXournal.Coroutine
import Application.HXournal.Device
import Application.HXournal.Iteratee
import Application.HXournal.GUI.Menu

import Graphics.UI.Gtk hiding (get,set)

import qualified Control.Monad.State as St
import Control.Monad.IO.Class
import Control.Monad.Coroutine

import Data.IORef
import qualified Data.Map as M


import Control.Category
import Data.Label
import Prelude hiding ((.),id)

import Text.Xournal.Type
import qualified Text.Xournal.Parse as P

startGUI :: FilePath -> IO () 
startGUI fname = do 
  initGUI
  devlst <- initDevice 
  
  canvas <- drawingAreaNew
  scrwin <- scrolledWindowNew Nothing Nothing 
  containerAdd scrwin canvas
  hadj <- adjustmentNew 0 0 500 100 200 200 
  vadj <- adjustmentNew 0 0 500 100 200 200 
  scrolledWindowSetHAdjustment scrwin hadj 
  scrolledWindowSetVAdjustment scrwin vadj 
  scrolledWindowSetPolicy scrwin PolicyAutomatic PolicyAutomatic 
  
  canvas2 <- drawingAreaNew 
  scrwin2 <- scrolledWindowNew Nothing Nothing 
  containerAdd scrwin2 canvas2
  hadj2 <- adjustmentNew 0 0 500 100 200 200
  vadj2 <- adjustmentNew 0 0 500 100 200 200
  scrolledWindowSetHAdjustment scrwin2 hadj2
  scrolledWindowSetVAdjustment scrwin2 vadj2
  scrolledWindowSetPolicy scrwin PolicyAutomatic PolicyAutomatic 

  xojcontent <- P.read_xournal fname 
  let xojWbbox = mkXournalBBoxFromXournal xojcontent 
  let Dim width height = pageDim . (!! 0) .  xournalPages $ xojcontent
      cinfo1 = set canvasId 1 
             . set drawArea canvas
             . set viewInfo (ViewInfo OnePage Original (0,0) (width,height))
             . set currentPageNum 0 
             . set horizAdjustment hadj 
             . set vertAdjustment vadj 
             $ emptyCanvasInfo
      cinfo2 = set canvasId 2 
             . set drawArea canvas2
             . set viewInfo (ViewInfo OnePage Original (0,0) (width,height))
             . set currentPageNum 0 
             . set horizAdjustment hadj2 
             . set vertAdjustment vadj2 
             $ emptyCanvasInfo
             
      cinfoMap = M.insert (get canvasId cinfo2) cinfo2
               $ M.insert (get canvasId cinfo1) cinfo1
               $ M.empty 
  let st = set xournalstate (ViewAppendState xojWbbox)
         . set canvasInfoMap cinfoMap 
         . set currentCanvas (get canvasId cinfo1)
         . set deviceList devlst 
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
  vpaned <- vPanedNew 
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

  containerAdd window vpaned
  
  boxPackStart vbox menubar PackNatural 0 
  boxPackStart vbox toolbar1 PackNatural 0
  boxPackStart vbox toolbar2 PackNatural 0 
  boxPackEnd vbox scrwin PackGrow 0 
  
  panedPack1 vpaned vbox True False 
  panedPack2 vpaned scrwin2 True False 
 
  cursorDot <- cursorNew BlankCursor  
  
  
  afterValueChanged hadj $ do 
    v <- adjustmentGetValue hadj 
    bouncecallback tref sref (HScrollBarMoved 1 v)
  afterValueChanged vadj $ do 
    v <- adjustmentGetValue vadj     
    bouncecallback tref sref (VScrollBarMoved 1 v)
  
  afterValueChanged hadj2 $ do 
    v <- adjustmentGetValue hadj2 
    bouncecallback tref sref (HScrollBarMoved 2 v)
  afterValueChanged vadj2 $ do 
    v <- adjustmentGetValue vadj2     
    bouncecallback tref sref (VScrollBarMoved 2 v)
  
  
  Just vscrbar <- scrolledWindowGetVScrollbar scrwin
  vscrbar `on` buttonPressEvent $ do 
    v <- liftIO $ adjustmentGetValue vadj 
    xstate <- liftIO (readIORef sref)
    let callbk = get callBack xstate
    liftIO (callbk (VScrollBarStart 1 v))
    return False
  vscrbar `on` buttonReleaseEvent $ do 
    v <- liftIO $ adjustmentGetValue vadj 
    xstate <- liftIO (readIORef sref)
    let callbk = get callBack xstate
    liftIO (callbk (VScrollBarEnd 1 v))
    return False
  
  Just vscrbar2 <- scrolledWindowGetVScrollbar scrwin2
  vscrbar2 `on` buttonPressEvent $ do 
    v <- liftIO $ adjustmentGetValue vadj2 
    xstate <- liftIO (readIORef sref)
    let callbk = get callBack xstate
    liftIO (callbk (VScrollBarStart 2 v))
    return False

  vscrbar2 `on` buttonReleaseEvent $ do 
    v <- liftIO $ adjustmentGetValue vadj2 
    xstate <- liftIO (readIORef sref)
    let callbk = get callBack xstate
    liftIO (callbk (VScrollBarEnd 2 v))
    return False

  canvas `on` sizeRequest $ return (Requisition 480 400)
  canvas2 `on` sizeRequest $ return (Requisition 480 400)
  
  canvas `on` buttonPressEvent $ tryEvent $ do 
    xstate <- liftIO (readIORef sref)
    let callbk = get callBack xstate
        dev = get deviceList xstate 
    p <- getPointer dev
    liftIO (callbk (PenDown 1 p))
  canvas2 `on` buttonPressEvent $ tryEvent $ do 
    xstate <- liftIO (readIORef sref)
    let callbk = get callBack xstate
        dev = get deviceList xstate 
    p <- getPointer dev
    liftIO (callbk (PenDown 2 p))
    
  canvas `on` configureEvent $ tryEvent $ do 
    (w,h) <- eventSize 
    liftIO $ bouncecallback tref sref 
                            (CanvasConfigure 1 (fromIntegral w) (fromIntegral h))
  canvas2 `on` configureEvent $ tryEvent $ do 
    (w,h) <- eventSize 
    liftIO $ bouncecallback tref sref 
                            (CanvasConfigure 2 (fromIntegral w) (fromIntegral h))

  canvas `on` buttonReleaseEvent $ tryEvent $ do 
    xstate <- liftIO (readIORef sref)
    let callbk = get callBack xstate
        dev = get deviceList xstate 
    p <- getPointer dev
    liftIO (callbk (PenUp 1 p))
  canvas2 `on` buttonReleaseEvent $ tryEvent $ do 
    xstate <- liftIO (readIORef sref)
    let callbk = get callBack xstate
        dev = get deviceList xstate 
    p <- getPointer dev
    liftIO (callbk (PenUp 2 p))
    
  canvas `on` exposeEvent $ tryEvent $ do 
    liftIO $ bouncecallback tref sref (UpdateCanvas 1)
  canvas2 `on` exposeEvent $ tryEvent $ do 
    liftIO $ bouncecallback tref sref (UpdateCanvas 2)
  
  canvas `on` enterNotifyEvent $ tryEvent $ do 
    win <- liftIO $ widgetGetDrawWindow canvas
    liftIO $ drawWindowSetCursor win (Just cursorDot)
    return ()
  canvas2 `on` enterNotifyEvent $ tryEvent $ do 
    win <- liftIO $ widgetGetDrawWindow canvas
    liftIO $ drawWindowSetCursor win (Just cursorDot)
    return ()

  
  widgetAddEvents canvas [PointerMotionMask,Button1MotionMask]
  widgetAddEvents canvas2 [PointerMotionMask,Button1MotionMask]  
  widgetSetExtensionEvents canvas [ExtensionEventsAll]
  widgetSetExtensionEvents canvas2 [ExtensionEventsAll]  
  
  onDestroy window mainQuit

  widgetShowAll window
  
  -- initialized
  bouncecallback tref sref Initialized     
  
  mainGUI 
  return ()
  
