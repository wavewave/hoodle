{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Default 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Default where

import           Control.Applicative hiding (empty)
import           Control.Concurrent 
import           Control.Concurrent.STM
import qualified Control.Exception as E
import           Control.Lens (over,view,set,at,(.~),(^.),_2)
import           Control.Monad.State hiding (mapM_)
import           Control.Monad.Trans.Reader (ReaderT(..))
import qualified Data.ByteString.Char8 as B
import           Data.Foldable (mapM_)
import qualified Data.IntMap as M
import           Data.IORef 
import qualified Data.List as L
import           Data.Maybe
import           Data.Sequence (Seq,viewl, ViewL(..))
import           Data.Time.Clock
import           Data.UUID
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk as Gtk hiding (get,set)
-- import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
-- import qualified Graphics.UI.Gtk.Poppler.Page as Poppler
import           System.Directory
import           System.Process 
-- from hoodle-platform
import           Control.Monad.Trans.Crtn.Driver
import           Control.Monad.Trans.Crtn.Object
import           Control.Monad.Trans.Crtn.Logger.Simple
import           Control.Monad.Trans.Crtn.Queue
import           Data.Hoodle.Simple (Dimension(..), Background(..))
import           Data.Hoodle.Generic
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Engine
import           Graphics.Hoodle.Render.Background
import           Graphics.Hoodle.Render.Type
-- from this package
import           Hoodle.Accessor
import           Hoodle.Coroutine.Callback
import           Hoodle.Coroutine.ContextMenu
import           Hoodle.Coroutine.Default.Menu
import           Hoodle.Coroutine.Dialog
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Eraser
import           Hoodle.Coroutine.File
import           Hoodle.Coroutine.Highlighter
import           Hoodle.Coroutine.Link
import           Hoodle.Coroutine.Mode
import           Hoodle.Coroutine.Page
import           Hoodle.Coroutine.Pen
import           Hoodle.Coroutine.Scroll
import           Hoodle.Coroutine.Select
import           Hoodle.Coroutine.TextInput 
import           Hoodle.Coroutine.VerticalSpace 
import           Hoodle.Coroutine.Window
import           Hoodle.Device
import           Hoodle.GUI.Menu
import           Hoodle.GUI.Reflect
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Window 
import           Hoodle.Script.Hook
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.Predefined
import           Hoodle.Type.Undo
import           Hoodle.Type.Window 
import           Hoodle.Type.Widget
import           Hoodle.Util
import           Hoodle.Widget.Dispatch 
import           Hoodle.Widget.PanZoom
#ifdef HUB
import           Hoodle.Coroutine.HubInternal
import           Hoodle.Coroutine.Socket
#endif
--
import Prelude hiding (mapM_)

-- |
initCoroutine :: DeviceList 
              -> Gtk.Window 
              -> Maybe Hook 
              -> Int -- ^ maxundo 
              -> (Bool,Bool,Bool,Bool) -- ^ (xinputbool,usepz,uselyr,varcsr)
              -> IO (EventVar,HoodleState,Gtk.UIManager,Gtk.VBox)
initCoroutine devlst window mhook maxundo (xinputbool,usepz,uselyr,varcsr) = do 
  evar <- newEmptyMVar  
  putMVar evar Nothing 
  let callback = eventHandler evar
  st0new <- set deviceList devlst  
            . set rootOfRootWindow window 
            . set callBack callback  
            <$> emptyHoodleState 
  (ui,uicompsighdlr) <- getMenuUI evar    
  let st1 = set gtkUIManager ui st0new
      initcvs = set (canvasWidgets.widgetConfig.doesUsePanZoomWidget) usepz
              . set (canvasWidgets.widgetConfig.doesUseLayerWidget) uselyr
              $ defaultCvsInfoSinglePage { _canvasId = 1 } 
      initcvsbox = CanvasSinglePage initcvs
      st2 = st1 # over (unitHoodles.currentUnit) ( set frameState (Node 1) 
                                                 . updateFromCanvasInfoAsCurrentCanvas initcvsbox             
                                                 . set cvsInfoMap M.empty )
      uhdl2 = view (unitHoodles.currentUnit) st2
  (uhdl3,cvs,_wconf) <- constructFrame st2 uhdl2 (view frameState uhdl2)
  (uhdl4,wconf') <- eventConnect st2 uhdl3 (view frameState uhdl3)
  notebook <- Gtk.notebookNew
  statusbar <- Gtk.statusbarNew
  let st4 = (unitHoodles.currentUnit .~ uhdl4) st2
      st5 = st4 # over (unitHoodles.currentUnit) ( set undoTable (emptyUndo maxundo)  
                                                 . set frameState wconf' 
                                                 . set rootWindow cvs 
                                                 . set (hoodleFileControl.hoodleFileName) Nothing)
                . set (settings.doesUseXInput) xinputbool 
                . set (settings.doesUseVariableCursor) varcsr
                . set hookSet mhook 
                . set rootNotebook notebook
                . set uiComponentSignalHandler uicompsighdlr 
                . set statusBar (Just statusbar)
  vbox <- Gtk.vBoxNew False 0 
  Gtk.containerAdd window vbox
  vboxcvs <- Gtk.vBoxNew False 0 
  -- Gtk.notebookAppendPage notebook vboxcvs  ("untitled" :: T.Text)
  (_,uuid,btn) <- createTab callback notebook vboxcvs
  Gtk.containerAdd vboxcvs (view (unitHoodles.currentUnit.rootWindow) st5)

  sigid <- notebook `Gtk.on` Gtk.switchPage $ \i -> callback (UsrEv (SwitchTab i)) 
  let st6 = ( (unitHoodles.currentUnit.unitUUID .~ uuid) 
            . (unitHoodles.currentUnit.unitButton .~ btn)
            . (uiComponentSignalHandler.switchTabSignal .~ Just sigid)) st5
      -- st6 = st5
      startingXstate = (unitHoodles.currentUnit.rootContainer .~ Gtk.castToBox vboxcvs) st6
      startworld = world startingXstate . ReaderT $ (\(Arg DoEvent ev) -> guiProcess ev)  
  putMVar evar . Just $ (driver simplelogger startworld)
  return (evar,startingXstate,ui,vbox)

-- | initialization according to the setting 
initialize :: Maybe (CanvasId,CanvasDimension) -> Bool -> AllEvent -> MainCoroutine (CanvasId, CanvasDimension)
initialize cvs isInitialized ev = do  
    case ev of 
      UsrEv (Initialized mfname) -> do 
        if isInitialized 
          then case cvs of
                 Nothing -> nextevent >>= initialize Nothing True . UsrEv
                 Just cvsi -> return cvsi
          else do
	    -- additional initialization goes here
	    xst1 <- get
	    let ui = xst1 ^. gtkUIManager
		cachevar = xst1 ^. renderCacheVar
		tvarpdf = xst1 ^. pdfRenderQueue
		tvargen = xst1 ^. genRenderQueue
	    doIOaction $ \evhandler -> do 
	      -- let handler = Gtk.postGUIAsync . evhandler . SysEv . RenderCacheUpdate
	      forkOn 2 $ pdfRendererMain (defaultHandler evhandler) tvarpdf
	      forkIO $ E.catch (genRendererMain cachevar (defaultHandler evhandler) tvargen) (\e -> print (e :: E.SomeException)) 
	      return (UsrEv ActionOrdered)
	    waitSomeEvent (\case ActionOrdered -> True ; _ -> False ) 
	    getFileContent mfname
	    -- 
	    xst2 <- get
	    let uhdl = view (unitHoodles.currentUnit) xst2
		hdlst = uhdl ^. hoodleModeState 
		cid = getCurrentCanvasId uhdl
	    callRenderer_ $ resetHoodleModeStateBuffers cid hdlst
	    pureUpdateUhdl (hoodleModeState .~ hdlst)
	    -- liftIO $ toggleSave ui False
	    liftIO $ reflectUIToggle ui "SAVEA" False
	    pureUpdateUhdl (isSaved .~ True)
            case cvs of
              Just cvsi -> return cvsi
              Nothing -> nextevent >>= initialize Nothing True . UsrEv
      UsrEv (CanvasConfigure cid w h) -> do
        nextevent >>= initialize (Just (cid,CanvasDimension (Dim w h))) isInitialized . UsrEv 
      _ -> case (cvs,isInitialized) of
             (Just cvsi,True) -> return cvsi
             _ -> nextevent >>= initialize cvs isInitialized . UsrEv
-- |
guiProcess :: AllEvent -> MainCoroutine ()
guiProcess ev = do 
    (cid,cdim) <- initialize Nothing False ev
    liftIO $ print (cid,cdim)
    changePage (const 0)
    reflectViewModeUI
    reflectPenModeUI
    reflectPenColorUI  
    reflectPenWidthUI
    reflectNewPageModeUI
    viewModeChange ToContSinglePage
    pageZoomChange FitWidth
#ifdef HUB
    startLinkReceiver
    socketConnect
#endif
    doCanvasConfigure cid cdim
    -- main loop 
    sequence_ (repeat dispatchMode)

-- | 
dispatchMode :: MainCoroutine () 
dispatchMode = (view (unitHoodles.currentUnit) <$> get) 
               >>= return . hoodleModeStateEither . view hoodleModeState
               >>= either (const viewAppendMode) (const selectMode)
                     
-- | 
viewAppendMode :: MainCoroutine () 
viewAppendMode = do 
  r1 <- nextevent 
  case r1 of 
    PenDown cid pbtn pcoord -> 
      widgetCheckPen cid pcoord $ do 
        ptype <- getPenType 
        case (ptype,pbtn) of 
          (PenWork,PenButton1) -> do
            r <- penStart cid pcoord
            case r of 
              Just (Just Nothing) -> do 
                pureUpdateUhdl (isOneTimeSelectMode .~ YesBeforeSelect)
                modeChange ToSelectMode
                selectLassoStart PenButton3 cid pcoord
              _ -> return ()
          (PenWork,PenButton2) -> eraserStart cid pcoord 
          (PenWork,PenButton3) -> do 
            pureUpdateUhdl (isOneTimeSelectMode .~ YesBeforeSelect)
            modeChange ToSelectMode
            selectLassoStart PenButton3 cid pcoord
          (PenWork,EraserButton) -> eraserStart cid pcoord
          (PenWork,_) -> return () 
          (EraserWork,_)      -> eraserStart cid pcoord 
          (HighlighterWork,_) -> do 
            r <- highlighterStart cid pcoord 
            case r of 
              Just (Just Nothing) -> do 
                pureUpdateUhdl (isOneTimeSelectMode .~ YesBeforeSelect)
                modeChange ToSelectMode
                selectLassoStart PenButton3 cid pcoord
              _ -> return ()
          (VerticalSpaceWork,PenButton1) -> verticalSpaceStart cid pcoord 
          (VerticalSpaceWork,_) -> return () 
    TouchDown cid pcoord -> touchStart cid pcoord 
    PenMove cid pcoord -> disableTouch >> notifyLink cid pcoord  
    _ -> defaultEventProcess r1


disableTouch :: MainCoroutine () 
disableTouch = do 
    xst <- get 
    let devlst = view deviceList xst 
    when (view (settings.doesUseTouch) xst) $ do         
      let nxst = set (settings.doesUseTouch) False xst 
      doIOaction_ $ do
        lensSetToggleUIForFlag "HANDA" (settings.doesUseTouch) nxst
        let touchstr = dev_touch_str devlst
        -- ad hoc
        when (touchstr /= "touch") $ do 
          readProcess "xinput" [ "disable", touchstr ] "" 
          return ()
      put nxst

-- |
selectMode :: MainCoroutine () 
selectMode = do 
  r1 <- nextevent 
  case r1 of 
    PenDown cid pbtn pcoord -> do 
      ptype <- liftM (view (selectInfo.selectType)) get
      case ptype of 
        SelectRectangleWork -> selectRectStart pbtn cid pcoord 
        SelectLassoWork -> selectLassoStart pbtn cid pcoord 
        _ -> return ()
    PenMove cid pcoord -> disableTouch >> notifyLink cid pcoord 
    TouchDown cid pcoord -> touchStart cid pcoord     
    PenColorChanged c -> do modify (penInfo.currentTool.penColor .~ c)
                            selectPenColorChanged c
    PenWidthChanged v -> do 
      w <- flip int2Point v . view (penInfo.penType) <$> get     
      modify (penInfo.currentTool.penWidth .~ w) 
      selectPenWidthChanged w 
    _ -> defaultEventProcess r1

-- |
defaultEventProcess :: UserEvent -> MainCoroutine ()
defaultEventProcess (UpdateCanvas cid) = invalidate cid   
defaultEventProcess (UpdateCanvasEfficient cid) = 
  invalidateInBBox Nothing Efficient cid   
defaultEventProcess (Menu m) = menuEventProcess m
defaultEventProcess (HScrollBarMoved cid v) = hscrollBarMoved cid v
defaultEventProcess (VScrollBarMoved cid v) = vscrollBarMoved cid v
defaultEventProcess (VScrollBarStart cid v) = vscrollStart cid v 
defaultEventProcess PaneMoveStart = paneMoveStart 
defaultEventProcess (CanvasConfigure cid w' h') =  
    doCanvasConfigure cid (CanvasDimension (Dim w' h'))
defaultEventProcess ToViewAppendMode = modeChange ToViewAppendMode
defaultEventProcess ToSelectMode = modeChange ToSelectMode 
defaultEventProcess ToSinglePage = viewModeChange ToSinglePage
defaultEventProcess ToContSinglePage = viewModeChange ToContSinglePage
defaultEventProcess (AssignPenMode t) =  
    case t of 
      Left pm -> do 
        modify (penInfo.penType .~ pm)
        modeChange ToViewAppendMode
      Right sm -> do 
        modify (selectInfo.selectType .~ sm)
        modeChange ToSelectMode 
defaultEventProcess (PenColorChanged c) = do 
    modify (penInfo.currentTool.penColor .~ c)
    reflectPenColorUI
defaultEventProcess (PenWidthChanged v) = do 
    st <- get 
    let ptype = view (penInfo.penType) st
    let w = int2Point ptype v
    let stNew = set (penInfo.currentTool.penWidth) w st 
    put stNew 
    reflectPenWidthUI
defaultEventProcess (BackgroundStyleChanged bsty) = do
    modify (backgroundStyle .~ bsty)
    uhdl <- view (unitHoodles.currentUnit) <$> get 
    let pgnum = view (currentCanvasInfo . unboxLens currentPageNum) uhdl
        hdl = getHoodle uhdl
        pgs = view gpages hdl 
        cpage = getPageFromGHoodleMap pgnum hdl
        cbkg = view gbackground cpage
        bstystr = convertBackgroundStyleToByteString bsty 
        -- for the time being, I replace any background to solid background
        dim = view gdimension cpage
        getnbkg' :: RBackground -> Background 
        getnbkg' (RBkgSmpl c _ _)     = Background "solid"  c bstystr
        getnbkg' (RBkgPDF _ _ _ _ _)  = Background "solid" "white" bstystr  
        getnbkg' (RBkgEmbedPDF _ _ _) = Background "solid" "white" bstystr  
        -- 
    liftIO $ putStrLn " defaultEventProcess: BackgroundStyleChanged HERE/ "

    callRenderer $ GotRBackground <$> evalStateT (cnstrctRBkg_StateT dim (getnbkg' cbkg)) Nothing
    RenderEv (GotRBackground nbkg) <- 
      waitSomeEvent (\case RenderEv (GotRBackground _) -> True ; _ -> False )
                   
    let npage = set gbackground nbkg cpage 
        npgs = set (at pgnum) (Just npage) pgs 
        nhdl = set gpages npgs hdl 
    modeChange ToViewAppendMode     
    pureUpdateUhdl (hoodleModeState .~ ViewAppendState nhdl)
    invalidateAll 
defaultEventProcess (AssignNewPageMode nmod) = modify (settings.newPageMode .~ nmod)
defaultEventProcess (GotContextMenuSignal ctxtmenu) = processContextMenu ctxtmenu
defaultEventProcess (GetHoodleFileInfo ref) = do 
  uhdl <- view (unitHoodles.currentUnit) <$> get
  let hdl = getHoodle uhdl
      uuid = B.unpack (view ghoodleID hdl)
  case view (hoodleFileControl.hoodleFileName) uhdl of 
    Nothing -> liftIO $ writeIORef ref Nothing
    Just fp -> liftIO $ writeIORef ref (Just (uuid ++ "," ++ fp))
defaultEventProcess (GetHoodleFileInfoFromTab uuidtab ref) = do 
  uhdlmap <- view (unitHoodles._2) <$> get
  let muhdl = (L.lookup uuidtab . map (\x -> (view unitUUID x,x)) . M.elems) uhdlmap
  case muhdl of 
    Nothing -> liftIO $ writeIORef ref Nothing
    Just uhdl -> do 
      let hdl = getHoodle uhdl
          uuid = B.unpack (view ghoodleID hdl)
      case view (hoodleFileControl.hoodleFileName) uhdl of 
        Nothing -> liftIO $ writeIORef ref Nothing
        Just fp -> liftIO $ writeIORef ref (Just (uuid ++ "," ++ fp))
defaultEventProcess (GotLink mstr (x,y)) = gotLink mstr (x,y)    
defaultEventProcess (Sync ctime) = do 
  xst <- get
  case view (unitHoodles.currentUnit.hoodleFileControl.lastSavedTime) xst of 
    Nothing -> return ()
    Just otime -> do 
      let dtime = diffUTCTime ctime otime
      if dtime < dtime_bound * 10 
        then return () 
        else 
          doIOaction $ \evhandler -> do 
            Gtk.postGUISync (evhandler (UsrEv FileReloadOrdered))
            return (UsrEv ActionOrdered)
defaultEventProcess FileReloadOrdered = fileReload 
defaultEventProcess (CustomKeyEvent str) = do
    if | str == "[]:\"Super_L\"" -> do  
           xst <- liftM (over (settings.doesUseTouch) not) get 
           put xst 
           doIOaction_ $ lensSetToggleUIForFlag "HANDA" (settings.doesUseTouch) xst
           toggleTouch
       | str == "[]:\"1\"" -> colorfunc ColorBlack
       | str == "[]:\"2\"" -> colorfunc ColorBlue 
       | str == "[]:\"3\"" -> colorfunc ColorRed
       | str == "[]:\"4\"" -> colorfunc ColorGreen
       | str == "[]:\"5\"" -> colorfunc ColorGray
       | str == "[]:\"6\"" -> colorfunc ColorLightBlue
       | str == "[]:\"7\"" -> colorfunc ColorLightGreen
       | str == "[]:\"8\"" -> colorfunc ColorMagenta
       | str == "[]:\"9\"" -> colorfunc ColorOrange
       | str == "[]:\"0\"" -> colorfunc ColorYellow
       | str == "[]:\"minus\"" -> colorfunc ColorWhite
       | str == "[]:\"a\"" -> toolfunc PenWork 
       | str == "[]:\"b\"" -> toolfunc HighlighterWork
       | str == "[]:\"c\"" -> toolfunc EraserWork
       | str == "[]:\"d\"" -> toolfunc VerticalSpaceWork
       | otherwise -> return ()
  where 
    colorfunc c = doIOaction $ \_evhandler -> return (UsrEv (PenColorChanged c))
    toolfunc t = doIOaction $ \_evhandler -> return (UsrEv (AssignPenMode (Left t)))
defaultEventProcess (SwitchTab i) = switchTab i
defaultEventProcess (CloseTab uuid) = findTab uuid >>= mapM_  (\x-> switchTab x >> askIfSave closeTab)
defaultEventProcess (OpenLink urlpath mid) = openLinkAction urlpath mid
#ifdef HUB
defaultEventProcess (NetworkProcess (NetworkReceived txt)) = networkReceived txt
defaultEventProcess (DBusEv (ImageFileDropped fname)) = embedImage fname
defaultEventProcess (DBusEv (DBusNetworkInput txt)) = dbusNetworkInput txt 
defaultEventProcess (DBusEv (GoToLink (docid,anchorid))) = goToAnchorPos docid anchorid
defaultEventProcess (DisconnectedHub tokfile (ofile,file) hinfo) = do
    b <- okCancelMessageBox "authentication failure! do you want to start from the beginning?"
    when b $ do
      r' :: Either E.SomeException () <- liftIO (E.try (removeFile tokfile))
      case r' of 
        Left _ ->  msgShout "DisconnectedHub" >>  return ()
        Right _ -> uploadWork (ofile,file) hinfo
#endif
defaultEventProcess ev = -- for debugging
                         do msgShout "--- no default ---"
                            msgShout (show ev)
                            msgShout "------------------"

