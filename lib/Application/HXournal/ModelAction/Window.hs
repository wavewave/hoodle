module Application.HXournal.ModelAction.Window where

import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Event
import Application.HXournal.Type.Window
import Application.HXournal.Type.XournalState
import Application.HXournal.Device
import Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk as Gtk (set)
import Control.Monad.Trans 
import Control.Category
import Data.Label
import Prelude hiding ((.),id)
import qualified Data.IntMap as M
import System.FilePath

setTitleFromFileName :: HXournalState -> IO () 
setTitleFromFileName xstate = do 
  case get currFileName xstate of
    Nothing -> Gtk.set (get rootOfRootWindow xstate) 
                       [ windowTitle := "untitled" ]
    Just filename -> Gtk.set (get rootOfRootWindow xstate) 
                             [ windowTitle := takeFileName filename] 

newCanvasId :: CanvasInfoMap -> CanvasId 
newCanvasId cmap = 
  let cids = M.keys cmap 
  in  (maximum cids) + 1  


initCanvasInfo :: HXournalState -> CanvasId -> IO CanvasInfo 
initCanvasInfo xstate cid = do 
    let callback = get callBack xstate
        dev = get deviceList xstate 
    canvas <- drawingAreaNew
    scrwin <- scrolledWindowNew Nothing Nothing 
    containerAdd scrwin canvas
    hadj <- adjustmentNew 0 0 500 100 200 200 
    vadj <- adjustmentNew 0 0 500 100 200 200 
    scrolledWindowSetHAdjustment scrwin hadj 
    scrolledWindowSetVAdjustment scrwin vadj 
    -- scrolledWindowSetPolicy scrwin PolicyAutomatic PolicyAutomatic 
    
    canvas `on` sizeRequest $ return (Requisition 480 400)    
    canvas `on` buttonPressEvent $ tryEvent $ do 
      p <- getPointer dev
      liftIO (callback (PenDown cid p))
    canvas `on` configureEvent $ tryEvent $ do 
      (w,h) <- eventSize 
      liftIO $ callback -- bouncecallback tref sref 
                 (CanvasConfigure cid (fromIntegral w) (fromIntegral h))
    canvas `on` buttonReleaseEvent $ tryEvent $ do 
      p <- getPointer dev
      liftIO (callback (PenUp cid p))
    canvas `on` exposeEvent $ tryEvent $ do 
      liftIO $ callback (UpdateCanvas cid) 

    {-
    canvas `on` enterNotifyEvent $ tryEvent $ do 
      win <- liftIO $ widgetGetDrawWindow canvas
      liftIO $ drawWindowSetCursor win (Just cursorDot)
      return ()
    -}  
    widgetAddEvents canvas [PointerMotionMask,Button1MotionMask]      
    -- widgetSetExtensionEvents canvas [ExtensionEventsAll]
    -- widgetSetExtensionEvents canvas 
    let ui = get gtkUIManager xstate 
    agr <- liftIO ( uiManagerGetActionGroups ui >>= \x ->
                      case x of 
                        [] -> error "No action group? "
                        y:_ -> return y )
    uxinputa <- liftIO (actionGroupGetAction agr "UXINPUTA" >>= \(Just x) -> 
                          return (castToToggleAction x) )
    b <- liftIO $ toggleActionGetActive uxinputa
    if b
      then widgetSetExtensionEvents canvas [ExtensionEventsAll]
      else widgetSetExtensionEvents canvas [ExtensionEventsNone]

    afterValueChanged hadj $ do 
      v <- adjustmentGetValue hadj 
      callback (HScrollBarMoved cid v)
    afterValueChanged vadj $ do 
      v <- adjustmentGetValue vadj     
      callback (VScrollBarMoved cid v)
    Just vscrbar <- scrolledWindowGetVScrollbar scrwin
    vscrbar `on` buttonPressEvent $ do 
      v <- liftIO $ adjustmentGetValue vadj 
      liftIO (callback (VScrollBarStart cid v))
      return False
    vscrbar `on` buttonReleaseEvent $ do 
      v <- liftIO $ adjustmentGetValue vadj 
      liftIO (callback (VScrollBarEnd cid v))
      return False
    return $ CanvasInfo cid canvas scrwin (error "no viewInfo") 0 (error "No page")  hadj vadj 
  
constructFrame :: WindowConfig -> CanvasInfoMap -> IO (Widget,WindowConfig)
constructFrame (Node cid) cmap = do 
    case (M.lookup cid cmap) of
      Nothing -> error $ "no such cid = " ++ show cid ++ " in constructFrame"
      Just cinfo -> return (castToWidget . get scrolledWindow $ cinfo, Node cid)
constructFrame (HSplit hpane wconf1 wconf2) cmap = do  
    (win1,wconf1') <- constructFrame wconf1 cmap
    (win2,wconf2') <- constructFrame wconf2 cmap 
    hpane' <- case hpane of
                Nothing -> hPanedNew 
                Just h -> return h
    panedPack1 hpane' win1 True False
    panedPack2 hpane' win2 True False
    widgetShowAll hpane' 
    return (castToWidget hpane', HSplit (Just hpane') wconf1' wconf2')
constructFrame (VSplit vpane wconf1 wconf2) cmap = do  
    (win1,wconf1') <- constructFrame wconf1 cmap
    (win2,wconf2') <- constructFrame wconf2 cmap 
    vpane' <- case vpane of 
                Nothing -> vPanedNew 
                Just v -> return v
    panedPack1 vpane' win1 True False
    panedPack2 vpane' win2 True False
    widgetShowAll vpane' 
    return (castToWidget vpane', VSplit (Just vpane') wconf1' wconf2')
  
  
removePanes :: WindowConfig -> IO WindowConfig
removePanes n@(Node _) = return n
removePanes (HSplit hpane wconf1 wconf2) = do 
    case hpane of 
      Just h -> do 
        panedGetChild1 h >>= \x -> case x of 
          Just c1 -> containerRemove h c1
          Nothing -> return ()
        panedGetChild2 h >>= \x -> case x of 
          Just c2 -> containerRemove h c2
          Nothing -> return ()
        widgetDestroy h
      Nothing -> return ()
    wconf1' <- removePanes wconf1   
    wconf2' <- removePanes wconf2 
    return (HSplit Nothing wconf1' wconf2')
removePanes (VSplit vpane wconf1 wconf2) = do 
    case vpane of 
      Just v -> do 
        panedGetChild1 v >>= \x -> case x of 
          Just c1 -> containerRemove v c1
          Nothing -> return ()
        panedGetChild2 v >>= \x -> case x of 
          Just c2 -> containerRemove v c2
          Nothing -> return ()
        widgetDestroy v
      Nothing -> return ()
    wconf1' <- removePanes wconf1 
    wconf2' <- removePanes wconf2 
    return (VSplit Nothing wconf1' wconf2')  
   
