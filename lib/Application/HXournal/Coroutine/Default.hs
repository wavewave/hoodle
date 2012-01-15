module Application.HXournal.Coroutine.Default where

import Graphics.UI.Gtk hiding (get,set)

import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Clipboard

import Application.HXournal.Accessor

import Application.HXournal.GUI.Menu
import Application.HXournal.Coroutine.Callback
import Application.HXournal.Coroutine.Commit
import Application.HXournal.Coroutine.Draw
import Application.HXournal.Coroutine.Pen
import Application.HXournal.Coroutine.Eraser
import Application.HXournal.Coroutine.Highlighter
import Application.HXournal.Coroutine.Scroll
import Application.HXournal.Coroutine.Page
import Application.HXournal.Coroutine.Select
import Application.HXournal.Coroutine.File
import Application.HXournal.Coroutine.Mode
import Application.HXournal.Coroutine.Window
import Application.HXournal.Coroutine.Network
import Application.HXournal.Coroutine.Layer 

import Application.HXournal.ModelAction.Window 
import Application.HXournal.Type.Window 
import Application.HXournal.Device
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import qualified Control.Monad.State as St 
import Control.Monad.Trans
import qualified Data.IntMap as M
import Data.Maybe
import Control.Category
import Data.Label
import Prelude hiding ((.), id)
import Data.IORef


import Data.Xournal.Generic

guiProcess :: MainCoroutine ()
guiProcess = do 
  initialize
  changePage (const 0)
  xstate <- getSt
  let cinfoMap  = get canvasInfoMap xstate
      assocs = M.toList cinfoMap 
      f (cid,cinfo) = do let canvas = get drawArea cinfo
                         (w',h') <- liftIO $ widgetGetSize canvas
                         defaultEventProcess (CanvasConfigure cid
                                                (fromIntegral w') 
                                                (fromIntegral h')) 
  mapM_ f assocs
  sequence_ (repeat dispatchMode)

initCoroutine :: DeviceList -> Window -> IO (TRef,SRef)
initCoroutine devlst window = do 
  let st0 = (emptyHXournalState :: HXournalState)
  sref <- newIORef st0
  tref <- newIORef (undefined :: SusAwait)
  (r,st') <- St.runStateT (resume guiProcess) st0 
  writeIORef sref st' 
  case r of 
    Left aw -> do 
      writeIORef tref aw 
    Right _ -> error "what?"

  let st0new = set deviceList devlst  
            . set rootOfRootWindow window 
            . set callBack (bouncecallback tref sref) 
            $ st' 
  writeIORef sref st0new            
  ui <- getMenuUI tref sref    
  putStrLn "hi"  
  let st1 = set gtkUIManager ui st0new

  initcvs <- initCanvasInfo st1 1 
  let initcmap = M.insert (get canvasId initcvs) initcvs M.empty
  let startingXstate = set currentCanvas (get canvasId initcvs)
                       . set canvasInfoMap initcmap 
                       . set frameState (Node 1)
                       $ st1
  writeIORef sref startingXstate   
  return (tref,sref)

initialize :: MainCoroutine ()
initialize = do ev <- await 
                liftIO $ putStrLn $ show ev 
                case ev of 
                  Initialized -> return () 
                  _ -> initialize


dispatchMode :: MainCoroutine () 
dispatchMode = do 
  xojstate <- return . get xournalstate =<< lift St.get
  case xojstate of 
    ViewAppendState _ -> viewAppendMode
    SelectState _ -> selectMode

viewAppendMode :: MainCoroutine () 
viewAppendMode = do 
  r1 <- await 
  case r1 of 
    PenDown cid pcoord -> do 
      ptype <- getPenType 
      case ptype of 
        PenWork         -> penStart cid pcoord 
        EraserWork      -> eraserStart cid pcoord 
        HighlighterWork -> highlighterStart pcoord
        _ -> return () 
    _ -> defaultEventProcess r1

selectMode :: MainCoroutine () 
selectMode = do 
  r1 <- await 
  case r1 of 
    PenDown cid pcoord -> do 
      ptype <- return . get (selectType.selectInfo) =<< lift St.get 
      case ptype of 
        SelectRectangleWork -> selectRectStart cid pcoord 
        _ -> return () 
    PenColorChanged c -> selectPenColorChanged c
    PenWidthChanged w -> selectPenWidthChanged w
    _ -> defaultEventProcess r1



defaultEventProcess :: MyEvent -> MainCoroutine ()
defaultEventProcess (UpdateCanvas cid) = invalidate cid   
defaultEventProcess (Menu m) = menuEventProcess m
defaultEventProcess (HScrollBarMoved cid v) = do 
    xstate <- getSt 
    let cinfoMap = get canvasInfoMap xstate
        maybeCvs = M.lookup cid cinfoMap 
    case maybeCvs of 
      Nothing -> return ()
      Just cvsInfo -> do 
        let vm_orig = get (viewPortOrigin.viewInfo) cvsInfo
        let cvsInfo' = set (viewPortOrigin.viewInfo) (v,snd vm_orig) 
                         $ cvsInfo
            xstate' = set currentCanvas cid 
                    . updateCanvasInfo cvsInfo' 
                    $ xstate
        lift . St.put $ xstate'
        invalidate cid
defaultEventProcess (VScrollBarMoved cid v) = do 
    xstate <- lift St.get 
    let cinfoMap = get canvasInfoMap xstate
        cvsInfo = case M.lookup cid cinfoMap of 
                     Nothing -> error "No such canvas in defaultEventProcess" 
                     Just cvs -> cvs
    let vm_orig = get (viewPortOrigin.viewInfo) cvsInfo
    let cvsInfo' = set (viewPortOrigin.viewInfo) (fst vm_orig,v)
                   $ cvsInfo 
        xstate' = set currentCanvas cid 
                  . updateCanvasInfo cvsInfo' $ xstate
    lift . St.put $ xstate'
    invalidate cid
defaultEventProcess (VScrollBarStart cid _v) = vscrollStart cid 
defaultEventProcess (CanvasConfigure cid _w' _h') = canvasZoomUpdate Nothing cid 
defaultEventProcess ToViewAppendMode = modeChange ToViewAppendMode
defaultEventProcess ToSelectMode = modeChange ToSelectMode 
defaultEventProcess _ = return ()

askQuitProgram :: MainCoroutine () 
askQuitProgram = do 
  dialog <- liftIO $ messageDialogNew Nothing [DialogModal] 
                       MessageQuestion ButtonsOkCancel 
                       "Current canvas is not saved yet. Will you close hxournal?" 
  res <- liftIO $ dialogRun dialog
  case res of
    ResponseOk -> do 
      liftIO $ widgetDestroy dialog
      liftIO $ mainQuit
    _ -> do 
      liftIO $ widgetDestroy dialog
      return ()

menuEventProcess :: MenuEvent -> MainCoroutine () 
menuEventProcess MenuQuit = do 
  xstate <- getSt
  liftIO $ putStrLn "MenuQuit called"
  if get isSaved xstate 
    then liftIO $ mainQuit
    else askQuitProgram
menuEventProcess MenuPreviousPage = changePage (\x->x-1)
menuEventProcess MenuNextPage =  changePage (+1)
menuEventProcess MenuFirstPage = changePage (const 0)
menuEventProcess MenuLastPage = do 
  xstate <- getSt
  let totalnumofpages = case get xournalstate xstate of 
                          ViewAppendState xoj ->  M.size . get g_pages $ xoj
                          SelectState txoj    -> M.size . gselectAll $ txoj
  changePage (const (totalnumofpages-1))
menuEventProcess MenuNewPageBefore = newPageBefore 
menuEventProcess MenuNew  = askIfSave fileNew 
menuEventProcess MenuAnnotatePDF = askIfSave fileAnnotatePDF
menuEventProcess MenuUndo = undo 
menuEventProcess MenuRedo = redo
menuEventProcess MenuOpen = askIfSave fileOpen
menuEventProcess MenuSave = fileSave 
menuEventProcess MenuSaveAs = fileSaveAs
menuEventProcess MenuCut = cutSelection
menuEventProcess MenuCopy = copySelection
menuEventProcess MenuPaste = pasteToSelection
menuEventProcess MenuDelete = deleteSelection
menuEventProcess MenuNetCopy = clipCopyToNetworkClipboard
menuEventProcess MenuNetPaste = clipPasteFromNetworkClipboard
menuEventProcess MenuNormalSize = pageZoomChange Original  
menuEventProcess MenuPageWidth = pageZoomChange FitWidth 
menuEventProcess MenuPageHeight = pageZoomChange FitHeight
menuEventProcess MenuHSplit = eitherSplit SplitHorizontal
menuEventProcess MenuVSplit = eitherSplit SplitVertical
menuEventProcess MenuDelCanvas = deleteCanvas
menuEventProcess MenuNewLayer = makeNewLayer 
menuEventProcess MenuNextLayer = gotoNextLayer 
menuEventProcess MenuPrevLayer = gotoPrevLayer
menuEventProcess MenuGotoLayer = startGotoLayerAt 
menuEventProcess MenuDeleteLayer = deleteCurrentLayer

menuEventProcess MenuUseXInput = do 
  xstate <- getSt 
  let ui = get gtkUIManager xstate 
  agr <- liftIO ( uiManagerGetActionGroups ui >>= \x ->
                    case x of 
                      [] -> error "No action group? "
                      y:_ -> return y )
  uxinputa <- liftIO (actionGroupGetAction agr "UXINPUTA" >>= \(Just x) -> 
                        return (castToToggleAction x) )
  b <- liftIO $ toggleActionGetActive uxinputa
  let cmap = get canvasInfoMap xstate
      canvases = map (get drawArea) . M.elems $ cmap 
  
  if b
    then mapM_ (\x->liftIO $ widgetSetExtensionEvents x [ExtensionEventsAll]) canvases
    else mapM_ (\x->liftIO $ widgetSetExtensionEvents x [ExtensionEventsNone] ) canvases
         
menuEventProcess m = liftIO $ putStrLn $ "not implemented " ++ show m 
