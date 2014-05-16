{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.ModelAction.Window 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.ModelAction.Window where

-- from other packages
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens (view)
import           Control.Monad
import           Control.Monad.Trans 
import qualified Data.IntMap as M
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           DBus
import           DBus.Client
import           Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk as Gtk (set)
import           System.FilePath
-- from this package
import           Hoodle.Device
import           Hoodle.Type.Canvas
import           Hoodle.Type.Event
import           Hoodle.Type.Window
import           Hoodle.Type.HoodleState
import           Hoodle.Util
-- 

getDBUSEvent :: (AllEvent -> IO ()) -> TVar Bool -> IO ()
getDBUSEvent callback tvar = do
    client <- connectSession
    requestName client "org.ianwookim" []
    forkIO $ void $ addMatch client matchAny { matchInterface = Just "org.ianwookim.hoodle"
                                             , matchMember = Just "filepath"
                                             }
               getImage
      
    forkIO $ void $ addMatch client matchAny { matchInterface = Just "org.ianwookim.hoodle"
                                             , matchMember = Just "latex"
                                             }
               getLaTeX
    forever getLine
  where getImage sig = do 
          let fps = mapMaybe fromVariant (signalBody sig) :: [T.Text]
          b <- atomically (readTVar tvar)  
          when ((not.null) fps && b) $ do  
            (postGUISync . callback . UsrEv . DBusEv . ImageFileDropped . T.unpack . head) 
              fps
            return ()
        getLaTeX sig = do 
          let latex = mapMaybe fromVariant (signalBody sig) :: [T.Text]
          b <- atomically (readTVar tvar)  
          when ((not.null) latex && b) $ do  
            (postGUISync . callback . UsrEv . DBusEv . DBusNetworkInput . head) 
              latex
            return ()

-- | set frame title according to file name
setTitleFromFileName :: HoodleState -> IO () 
setTitleFromFileName xstate = do 
  case view (hoodleFileControl.hoodleFileName) xstate of
    Nothing -> Gtk.set (view rootOfRootWindow xstate) 
                       [ windowTitle := "untitled" ]
    Just filename -> Gtk.set (view rootOfRootWindow xstate) 
                             [ windowTitle := takeFileName filename] 

-- | 
newCanvasId :: CanvasInfoMap -> CanvasId 
newCanvasId cmap = 
  let cids = M.keys cmap 
  in  (maximum cids) + 1  

-- | initialize CanvasInfo with creating windows and connect events
initCanvasInfo :: HoodleState -> CanvasId -> IO (CanvasInfo a)
initCanvasInfo xstate cid = 
  minimalCanvasInfo xstate cid >>= connectDefaultEventCanvasInfo xstate
  

-- | only creating windows 
minimalCanvasInfo :: HoodleState -> CanvasId -> IO (CanvasInfo a)
minimalCanvasInfo _xstate cid = do 
    canvas <- drawingAreaNew
    scrwin <- scrolledWindowNew Nothing Nothing 
    containerAdd scrwin canvas
    hadj <- adjustmentNew 0 0 500 100 200 200 
    vadj <- adjustmentNew 0 0 500 100 200 200 
    scrolledWindowSetHAdjustment scrwin hadj 
    scrolledWindowSetVAdjustment scrwin vadj 
    return $ CanvasInfo cid canvas Nothing scrwin (error "no viewInfo" :: ViewInfo a) 0 hadj vadj Nothing Nothing defaultCanvasWidgets Nothing 


-- | only connect events 

connectDefaultEventCanvasInfo 
  :: HoodleState -> CanvasInfo a -> IO (CanvasInfo a )
connectDefaultEventCanvasInfo xstate cinfo = do 
    let callback = view callBack xstate
        dev = view deviceList xstate 
        canvas = _drawArea cinfo 
        cid = _canvasId cinfo 
        scrwin = _scrolledWindow cinfo
        hadj = _horizAdjustment cinfo 
        vadj = _vertAdjustment cinfo 

    widgetSetCanFocus canvas True 
    widgetGrabFocus canvas     
    
    _sizereq <- canvas `on` sizeRequest $ return (Requisition 800 400)    
    
    _keyevent <- canvas `on` keyPressEvent $ tryEvent $ do 
      m <- eventModifier
      n <- eventKeyName 
      let keystr = show m ++ ":" ++ show n
      liftIO $ (callback (UsrEv (CustomKeyEvent keystr)))
    _bpevent <- canvas `on` buttonPressEvent $ tryEvent $ do 
                 liftIO $ widgetGrabFocus canvas 
                 (mbtn,mp) <- getPointer dev
                 case mp of 
                   Nothing -> return ()
                   Just p -> do 
                     let pbtn = maybe PenButton1 id mbtn
                     case pbtn of 
                       TouchButton -> liftIO (callback (UsrEv (TouchDown cid p)))
                       _ -> liftIO (callback (UsrEv (PenDown cid pbtn p)))
    _confevent <- canvas `on` configureEvent $ tryEvent $ do 
      
                   (w,h) <- eventSize 
                   liftIO $ callback (UsrEv (CanvasConfigure cid (fromIntegral w) (fromIntegral h)))
    _brevent <- canvas `on` buttonReleaseEvent $ tryEvent $ do 
                 (mbtn,mp) <- getPointer dev
                 case mp of 
                   Nothing -> return () 
                   Just p -> do
                     let pbtn = maybe PenButton1 id mbtn
                     case pbtn of 
                       TouchButton -> (liftIO . callback . UsrEv) (TouchUp cid p)
                       _ -> (liftIO . callback . UsrEv) (PenUp cid p)
                       
    tvar <- newTVarIO False 
    forkIO $ getDBUSEvent callback tvar
    _focus <- canvas `on` focusInEvent $ tryEvent $ liftIO $ do
                atomically (writeTVar tvar True)
                widgetGrabFocus canvas 
    _focusout <- canvas `on` focusOutEvent $ tryEvent $ liftIO $ atomically (writeTVar tvar False)
    _exposeev <- canvas `on` exposeEvent $ tryEvent $ do 
      liftIO $ widgetGrabFocus canvas       
      (liftIO . callback . UsrEv) (UpdateCanvas cid) 
    canvas `on` motionNotifyEvent $ tryEvent $ do 
      (mbtn,mp) <- getPointer dev
      case mp of 
        Nothing -> return ()
        Just p -> do 
          let pbtn = maybe PenButton1 id mbtn      
          case pbtn of 
            TouchButton -> (liftIO . callback . UsrEv) (TouchMove cid p) 
            _ -> (liftIO . callback . UsrEv) (PenMove cid p)

    -- drag and drop setting
    dragDestSet canvas [DestDefaultMotion, DestDefaultDrop] [ActionCopy]
    dragDestAddTextTargets canvas
    canvas `on` dragDataReceived $ \_dc pos _i _ts -> do 
      s <- selectionDataGetText 
      (liftIO . callback . UsrEv) (GotLink s pos)
      
    widgetAddEvents canvas [PointerMotionMask,Button1MotionMask,KeyPressMask]      
    let ui = view gtkUIManager xstate 
    agr <- liftIO ( uiManagerGetActionGroups ui >>= \x ->
                      case x of 
                        [] -> error "No action group? "
                        y:_ -> return y )
    uxinputa <- liftIO (actionGroupGetAction agr "UXINPUTA" >>= \(Just x) -> 
                          return (castToToggleAction x) )
    b <- liftIO $ toggleActionGetActive uxinputa
    if b then widgetSetExtensionEvents canvas [ExtensionEventsAll]
         else widgetSetExtensionEvents canvas [ExtensionEventsNone]
    hadjconnid <- afterValueChanged hadj $ do 
                    v <- adjustmentGetValue hadj 
                    (callback . UsrEv) (HScrollBarMoved cid v)
    vadjconnid <- afterValueChanged vadj $ do 
                    v <- adjustmentGetValue vadj     
                    (callback . UsrEv) (VScrollBarMoved cid v)
    Just vscrbar <- scrolledWindowGetVScrollbar scrwin
    _bpevtvscrbar <- vscrbar `on` buttonPressEvent $ do 
                      v <- liftIO $ adjustmentGetValue vadj 
                      liftIO ((callback . UsrEv) (VScrollBarStart cid v))
                      return False
    _brevtvscrbar <- vscrbar `on` buttonReleaseEvent $ do 
                      v <- liftIO $ adjustmentGetValue vadj 
                      liftIO ((callback . UsrEv) (VScrollBarEnd cid v))
                      return False
    return $ cinfo { _horizAdjConnId = Just hadjconnid
                   , _vertAdjConnId = Just vadjconnid }
    
-- | recreate windows from old canvas info but no event connect

reinitCanvasInfoStage1 
  :: HoodleState -> CanvasInfo a -> IO (CanvasInfo a)
reinitCanvasInfoStage1 xstate oldcinfo = do 
  let cid = view canvasId oldcinfo 
  newcinfo <- minimalCanvasInfo xstate cid      
  return $ newcinfo { _viewInfo = _viewInfo oldcinfo 
                    , _currentPageNum = _currentPageNum oldcinfo 
                    } 

    
-- | event connect

reinitCanvasInfoStage2 
  :: HoodleState -> CanvasInfo a -> IO (CanvasInfo a)
reinitCanvasInfoStage2 = connectDefaultEventCanvasInfo
    
-- | event connecting for all windows                          
                         
eventConnect :: HoodleState -> WindowConfig 
                -> IO (HoodleState,WindowConfig)
eventConnect xstate (Node cid) = do 
    let cmap = getCanvasInfoMap xstate 
        cinfobox = maybeError' "eventConnect" $ M.lookup cid cmap
    ncinfobox <- forBoth unboxBiXform (reinitCanvasInfoStage2 xstate) cinfobox
    let xstate' = updateFromCanvasInfoAsCurrentCanvas ncinfobox xstate
    return (xstate', Node cid)        
eventConnect xstate (HSplit wconf1 wconf2) = do  
    (xstate',wconf1') <- eventConnect xstate wconf1 
    (xstate'',wconf2') <- eventConnect xstate' wconf2 
    return (xstate'',HSplit wconf1' wconf2')
eventConnect xstate (VSplit wconf1 wconf2) = do  
    (xstate',wconf1') <- eventConnect xstate wconf1 
    (xstate'',wconf2') <- eventConnect xstate' wconf2 
    return (xstate'',VSplit wconf1' wconf2')
    
-- | default construct frame     

constructFrame :: HoodleState -> WindowConfig 
                  -> IO (HoodleState,Widget,WindowConfig)
constructFrame hst wcfg = constructFrame' (CanvasSinglePage defaultCvsInfoSinglePage) hst wcfg 



-- | construct frames with template

constructFrame' :: CanvasInfoBox -> 
                   HoodleState -> WindowConfig 
                   -> IO (HoodleState,Widget,WindowConfig)
constructFrame' template oxstate (Node cid) = do 
    let ocmap = getCanvasInfoMap oxstate 
    (cinfobox,_cmap,xstate) <- case M.lookup cid ocmap of 
      Just cinfobox' -> return (cinfobox',ocmap,oxstate)
      Nothing -> do 
        let cinfobox' = setCanvasId cid template 
            cmap' = M.insert cid cinfobox' ocmap
            xstate' = maybe oxstate id (setCanvasInfoMap cmap' oxstate)
        return (cinfobox',cmap',xstate')
    ncinfobox <- forBoth unboxBiXform (reinitCanvasInfoStage1 xstate) cinfobox
    let xstate' = updateFromCanvasInfoAsCurrentCanvas ncinfobox xstate
    let scrwin = forBoth' unboxBiAct (castToWidget.view scrolledWindow) ncinfobox
    return (xstate', scrwin, Node cid)
constructFrame' template xstate (HSplit wconf1 wconf2) = do  
    (xstate',win1,wconf1') <- constructFrame' template xstate wconf1     
    (xstate'',win2,wconf2') <- constructFrame' template xstate' wconf2 
    let callback = view callBack xstate'' 
    hpane' <- hPanedNew
    hpane' `on` buttonPressEvent $ do 
      liftIO ((callback . UsrEv) PaneMoveStart)
      return False 
    hpane' `on` buttonReleaseEvent $ do 
      liftIO ((callback . UsrEv) PaneMoveEnd)
      return False       
    panedPack1 hpane' win1 True False
    panedPack2 hpane' win2 True False
    widgetShowAll hpane' 
    return (xstate'',castToWidget hpane', HSplit wconf1' wconf2')
constructFrame' template xstate (VSplit wconf1 wconf2) = do  
    (xstate',win1,wconf1') <- constructFrame' template xstate wconf1 
    (xstate'',win2,wconf2') <- constructFrame' template xstate' wconf2 
    let callback = view callBack xstate''     
    vpane' <- vPanedNew 
    vpane' `on` buttonPressEvent $ do 
      liftIO ((callback . UsrEv) PaneMoveStart)
      return False 
    vpane' `on` buttonReleaseEvent $ do 
      liftIO ((callback . UsrEv) PaneMoveEnd)
      return False 
    panedPack1 vpane' win1 True False
    panedPack2 vpane' win2 True False
    widgetShowAll vpane' 
    return (xstate'',castToWidget vpane', VSplit wconf1' wconf2')
  