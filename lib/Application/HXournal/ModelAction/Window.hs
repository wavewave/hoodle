module Application.HXournal.ModelAction.Window where

import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Event
import Application.HXournal.Type.Window
import Application.HXournal.Type.XournalState
import Application.HXournal.Device
import Application.HXournal.Iteratee.Default
import Application.HXournal.Coroutine

import Graphics.UI.Gtk hiding (get,set)

import Control.Monad.Coroutine
import Control.Monad.Trans 
import qualified Control.Monad.State as St

import Control.Category
import Data.Label
import Prelude hiding ((.),id)

import qualified Data.IntMap as M
import Data.IORef 

initCoroutine :: DeviceList -> IO (TRef,SRef)
initCoroutine devlst = do 
  sref <- newIORef (emptyHXournalState :: HXournalState)
  tref <- newIORef (undefined :: SusAwait)
  initcvs <- initCanvasInfo tref sref 1 
  let initcmap = M.insert (get canvasId initcvs) initcvs M.empty
  let startingXstate = set deviceList devlst 
                       . set currentCanvas (get canvasId initcvs)
                       . set canvasInfoMap initcmap 
                       $ emptyHXournalState
  (r,st') <- St.runStateT (resume guiProcess) startingXstate 
  writeIORef sref st' 
  case r of 
    Left aw -> do 
      writeIORef tref aw 
    Right _ -> error "what?"
  return (tref,sref)

initCanvasInfo :: TRef -> SRef -> CanvasId -> IO CanvasInfo 
initCanvasInfo tref sref cid = do 
    canvas <- drawingAreaNew
    scrwin <- scrolledWindowNew Nothing Nothing 
    containerAdd scrwin canvas
    hadj <- adjustmentNew 0 0 500 100 200 200 
    vadj <- adjustmentNew 0 0 500 100 200 200 
    scrolledWindowSetHAdjustment scrwin hadj 
    scrolledWindowSetVAdjustment scrwin vadj 
    scrolledWindowSetPolicy scrwin PolicyAutomatic PolicyAutomatic 
    
    canvas `on` sizeRequest $ return (Requisition 480 400)    
    canvas `on` buttonPressEvent $ tryEvent $ do 
      xstate <- liftIO (readIORef sref)
      let callbk = get callBack xstate
          dev = get deviceList xstate 
      p <- getPointer dev
      liftIO (callbk (PenDown cid p))
    canvas `on` configureEvent $ tryEvent $ do 
      (w,h) <- eventSize 
      liftIO $ bouncecallback tref sref 
                 (CanvasConfigure cid (fromIntegral w) (fromIntegral h))
    canvas `on` buttonReleaseEvent $ tryEvent $ do 
      xstate <- liftIO (readIORef sref)
      let callbk = get callBack xstate
          dev = get deviceList xstate 
      p <- getPointer dev
      liftIO (callbk (PenUp cid p))
    canvas `on` exposeEvent $ tryEvent $ do 
      liftIO $ bouncecallback tref sref (UpdateCanvas cid)
    {-
    canvas `on` enterNotifyEvent $ tryEvent $ do 
      win <- liftIO $ widgetGetDrawWindow canvas
      liftIO $ drawWindowSetCursor win (Just cursorDot)
      return ()
    -}  
    widgetAddEvents canvas [PointerMotionMask,Button1MotionMask]      
    widgetSetExtensionEvents canvas [ExtensionEventsAll]
    

    afterValueChanged hadj $ do 
      v <- adjustmentGetValue hadj 
      bouncecallback tref sref (HScrollBarMoved cid v)
    afterValueChanged vadj $ do 
      v <- adjustmentGetValue vadj     
      bouncecallback tref sref (VScrollBarMoved cid v)
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
  
    return $ CanvasInfo cid canvas scrwin (error "no viewInfo") 0 (error "No page")  hadj vadj 
  
constructFrame :: WindowConfig -> CanvasInfoMap -> IO Widget
constructFrame (Node cid) cmap = do 
  case (M.lookup cid cmap) of
    Nothing -> error $ "no such cid = " ++ show cid ++ " in constructFrame"
    Just cinfo -> return . castToWidget . get scrolledWindow $ cinfo
constructFrame (HSplit wconf1 wconf2) cmap = do  
  win1 <- constructFrame wconf1 cmap
  win2 <- constructFrame wconf2 cmap 
  hpaned <- hPanedNew 
  panedPack1 hpaned win1 True False
  panedPack2 hpaned win2 True False
  return (castToWidget hpaned)
constructFrame (VSplit wconf1 wconf2) cmap = do  
  win1 <- constructFrame wconf1 cmap
  win2 <- constructFrame wconf2 cmap 
  vpaned <- vPanedNew 
  panedPack1 vpaned win1 True False
  panedPack2 vpaned win2 True False
  return (castToWidget vpaned)