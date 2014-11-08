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
import           Data.IORef (newIORef, readIORef)
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           Data.Traversable (traverse)
import           Data.UUID (UUID)
import           Data.UUID.V4
import           DBus hiding (UUID)
import           DBus.Client
import           Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk as Gtk
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
  case view (unitHoodles.currentUnit.hoodleFileControl.hoodleFileName) xstate of
    Nothing -> Gtk.set (view rootOfRootWindow xstate) 
                       [ windowTitle := ("untitled" :: String) ]
    Just filename -> Gtk.set (view rootOfRootWindow xstate) 
                             [ windowTitle := takeFileName filename] 

-- | 
newCanvasId :: CanvasInfoMap -> CanvasId 
newCanvasId cmap = 
  let cids = M.keys cmap 
  in  (maximum cids) + 1  

-- | initialize CanvasInfo with creating windows and connect events
initCanvasInfo :: HoodleState -> UnitHoodle -> CanvasId -> IO (CanvasInfo a)
initCanvasInfo xstate uhdl cid = 
  minimalCanvasInfo cid >>= connectDefaultEventCanvasInfo xstate uhdl
  

-- | only creating windows 
minimalCanvasInfo :: CanvasId -> IO (CanvasInfo a)
minimalCanvasInfo cid = do
    canvas <- drawingAreaNew
    scrwin <- scrolledWindowNew Nothing Nothing 
    containerAdd scrwin canvas
    hadj <- adjustmentNew 0 0 500 100 200 200 
    vadj <- adjustmentNew 0 0 500 100 200 200 
    scrolledWindowSetHAdjustment scrwin hadj 
    scrolledWindowSetVAdjustment scrwin vadj 
    putStrLn "this is called"
    return $ CanvasInfo cid canvas Nothing scrwin (error "no viewInfo" :: ViewInfo a) 0 hadj vadj Nothing Nothing defaultCanvasWidgets Nothing 


-- | only connect events 
connectDefaultEventCanvasInfo 
  :: HoodleState -> UnitHoodle -> CanvasInfo a -> IO (CanvasInfo a )
connectDefaultEventCanvasInfo xstate uhdl cinfo = do 
    let callback = view callBack xstate
        ui = view gtkUIManager xstate 
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
    agr <- liftIO ( uiManagerGetActionGroups ui >>= \x ->
                      case x of 
                        [] -> error "No action group? "
                        y:_ -> return y )
    uxinputa <- liftIO (actionGroupGetAction agr ("UXINPUTA" :: String) >>= \(Just x) -> 
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
  :: UnitHoodle -> CanvasInfo a -> IO (CanvasInfo a)
reinitCanvasInfoStage1 uhdl oldcinfo = do 
  let cid = view canvasId oldcinfo 
  newcinfo <- minimalCanvasInfo cid      
  return $ newcinfo { _viewInfo = _viewInfo oldcinfo 
                    , _currentPageNum = _currentPageNum oldcinfo 
                    } 

    
-- | event connect
reinitCanvasInfoStage2 
  :: HoodleState -> UnitHoodle -> CanvasInfo a -> IO (CanvasInfo a)
reinitCanvasInfoStage2 = connectDefaultEventCanvasInfo 
    
-- | event connecting for all windows                          
eventConnect :: HoodleState -> UnitHoodle -> WindowConfig -> IO (UnitHoodle,WindowConfig)
eventConnect xst uhdl (Node cid) = do 
    let cmap = view cvsInfoMap uhdl 
        cinfobox = maybeError' "eventConnect" $ M.lookup cid cmap
    ncinfobox <- forBoth unboxBiXform (reinitCanvasInfoStage2 xst uhdl) cinfobox
    let uhdl' = updateFromCanvasInfoAsCurrentCanvas ncinfobox uhdl
    return (uhdl', Node cid)        
eventConnect xst uhdl (HSplit wconf1 wconf2) = do  
    (uhdl',wconf1') <- eventConnect xst uhdl wconf1 
    (uhdl'',wconf2') <- eventConnect xst uhdl' wconf2 
    return (uhdl'',HSplit wconf1' wconf2')
eventConnect xst uhdl (VSplit wconf1 wconf2) = do  
    (uhdl',wconf1') <- eventConnect xst uhdl wconf1 
    (uhdl'',wconf2') <- eventConnect xst uhdl' wconf2 
    return (uhdl'',VSplit wconf1' wconf2')
    
-- | default construct frame     
constructFrame :: HoodleState 
               -> UnitHoodle -> WindowConfig 
               -> IO (UnitHoodle,Widget,WindowConfig)
constructFrame xst uhdl wcfg = 
    let callback = view callBack xst
    in constructFrame' callback (CanvasSinglePage defaultCvsInfoSinglePage) uhdl wcfg 

-- | construct frames with template
constructFrame' :: (AllEvent -> IO ()) -> CanvasInfoBox -> UnitHoodle -> WindowConfig 
                -> IO (UnitHoodle,Widget,WindowConfig)
constructFrame' _callback template ouhdl (Node cid) = do 
    let ocmap = view cvsInfoMap ouhdl
    (cinfobox,_cmap,uhdl) <- case M.lookup cid ocmap of 
      Just cinfobox' -> return (cinfobox',ocmap,ouhdl)
      Nothing -> do 
        let cinfobox' = setCanvasId cid template 
            cmap' = M.insert cid cinfobox' ocmap
            uhdl' = maybe ouhdl id (setCanvasInfoMap cmap' ouhdl)
        return (cinfobox',cmap',uhdl')
    ncinfobox <- forBoth unboxBiXform (reinitCanvasInfoStage1 uhdl) cinfobox
    let uhdl' = updateFromCanvasInfoAsCurrentCanvas ncinfobox uhdl
    print "hi"
    forBoth' unboxBiAct (putStrLn <=< widgetGetName . view drawArea) ncinfobox
    let scrwin = forBoth' unboxBiAct (castToWidget.view scrolledWindow) ncinfobox
    return (uhdl', scrwin, Node cid)
constructFrame' callback template uhdl (HSplit wconf1 wconf2) = do  
    (uhdl',win1,wconf1') <- constructFrame' callback template uhdl wconf1
    (uhdl'',win2,wconf2') <- constructFrame' callback template uhdl' wconf2 
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
    return (uhdl'',castToWidget hpane', HSplit wconf1' wconf2')
constructFrame' callback template uhdl (VSplit wconf1 wconf2) = do  
    (uhdl',win1,wconf1') <- constructFrame' callback template uhdl wconf1 
    (uhdl'',win2,wconf2') <- constructFrame' callback template uhdl' wconf2 
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
    return (uhdl'',castToWidget vpane', VSplit wconf1' wconf2')
  
registerFrameToContainer :: Gtk.Window -> Gtk.Box -> Gtk.Widget -> IO ()
registerFrameToContainer rtrwin rtcntr win = do
    boxPackEnd rtcntr win PackGrow 0 
   --  widgetShowAll rtcntr  
    widgetShowAll rtrwin
    widgetQueueDraw rtrwin


createTab :: (AllEvent -> IO ()) -> Gtk.Notebook -> Gtk.VBox -> IO (Int,UUID)
createTab callback notebook vboxcvs = do
    hbox <- Gtk.hBoxNew False 0 
    ebox <- Gtk.eventBoxNew
    label <- Gtk.labelNew (Just "hello" :: Maybe String)
    containerAdd ebox label
    Gtk.dragSourceSet ebox [Gtk.Button1] [Gtk.ActionCopy]
    Gtk.dragSourceSetIconStock ebox Gtk.stockIndex
    Gtk.dragSourceAddTextTargets ebox
    button <- Gtk.buttonNewWithLabel ("X" :: String)
    Gtk.boxPackStart hbox {- label -} ebox Gtk.PackNatural 0
    Gtk.boxPackStart hbox button Gtk.PackNatural 0 
    Gtk.widgetShowAll hbox
    mlabel <- Gtk.labelNew (Nothing :: Maybe String)
    n <- Gtk.notebookAppendPageMenu notebook vboxcvs hbox mlabel -- "undefined"
    uuid <- nextRandom
    button `Gtk.on` Gtk.buttonActivated $ callback (UsrEv (CloseTab uuid))

    ebox `on` Gtk.dragBegin $ \_dc -> do 
      liftIO $ putStrLn "dragging"
    ebox `on` Gtk.dragDataGet $ \_dc _iid _ts -> do
      minfo <- liftIO $ do 
        ref <- newIORef (Nothing :: Maybe String)
        callback (UsrEv (GetHoodleFileInfoFromTab uuid ref))
        readIORef ref
      liftIO $ print minfo
 
      traverse Gtk.selectionDataSetText minfo >> return ()



    return (n,uuid)
