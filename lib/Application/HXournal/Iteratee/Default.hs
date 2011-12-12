module Application.HXournal.Iteratee.Default where

import Graphics.UI.Gtk hiding (get,set)

import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Clipboard
import Application.HXournal.Draw
import Application.HXournal.Accessor

import Application.HXournal.Coroutine 
import Application.HXournal.Iteratee.Draw 
import Application.HXournal.Iteratee.Pen 
import Application.HXournal.Iteratee.Eraser
import Application.HXournal.Iteratee.Highlighter
import Application.HXournal.Iteratee.Scroll
import Application.HXournal.Iteratee.Page
import Application.HXournal.Iteratee.Select
import Application.HXournal.Iteratee.File
import Application.HXournal.Iteratee.Mode
import Application.HXournal.Iteratee.Window 

import Application.HXournal.ModelAction.Adjustment
import Application.HXournal.ModelAction.Page
import Application.HXournal.ModelAction.Window 
import Application.HXournal.Builder 

import Application.HXournal.Type.Window 
import Application.HXournal.Device


import Control.Applicative 
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import qualified Control.Monad.State as St 

import Control.Monad.Trans

import qualified Data.IntMap as M
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import Control.Category
import Data.Label
import Prelude hiding ((.), id)

import Text.Xournal.Type 
import Graphics.Xournal.Type
import Graphics.Xournal.Type.Map
import Graphics.Xournal.Type.Select

import Data.IORef

guiProcess :: Iteratee MyEvent XournalStateIO () 
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

initCoroutine :: DeviceList -> IO (TRef,SRef)
initCoroutine devlst = do 
  let st0 = (emptyHXournalState :: HXournalState)
  sref <- newIORef st0
  tref <- newIORef (undefined :: SusAwait)
  let st1 = set deviceList devlst  
            . set callBack (bouncecallback tref sref) $ st0 
  initcvs <- initCanvasInfo st1 1 
  let initcmap = M.insert (get canvasId initcvs) initcvs M.empty
  let startingXstate = set currentCanvas (get canvasId initcvs)
                       . set canvasInfoMap initcmap 
                       . set frameState (Node 1)
                       $ st1
  (r,st') <- St.runStateT (resume guiProcess) startingXstate 
  writeIORef sref st' 
  case r of 
    Left aw -> do 
      writeIORef tref aw 
    Right _ -> error "what?"
  return (tref,sref)

initialize :: Iteratee MyEvent XournalStateIO ()
initialize = do ev <- await 
                liftIO $ putStrLn $ show ev 
                case ev of 
                  Initialized -> return () 
                  _ -> initialize


dispatchMode :: Iteratee MyEvent XournalStateIO ()
dispatchMode = do 
  xojstate <- return . get xournalstate =<< lift St.get
  case xojstate of 
    ViewAppendState _ -> viewAppendMode
    SelectState _ -> selectMode

viewAppendMode :: Iteratee MyEvent XournalStateIO ()
viewAppendMode = do 
  r1 <- await 
  case r1 of 
    PenDown cid pcoord -> do 
      ptype <- getPenType 
      case ptype of 
        PenWork         -> penStart cid pcoord 
        EraserWork      -> eraserStart cid pcoord 
        HighlighterWork -> highlighterStart pcoord 
    _ -> defaultEventProcess r1

selectMode :: Iteratee MyEvent XournalStateIO ()
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



defaultEventProcess :: MyEvent -> Iteratee MyEvent XournalStateIO () 
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
defaultEventProcess (VScrollBarStart cid v) = vscrollStart cid 
defaultEventProcess (CanvasConfigure cid w' h') = do 
    xstate <- getSt
    let cinfoMap = get canvasInfoMap xstate
    case M.lookup cid cinfoMap of 
      Nothing -> return () -- error $ show ( M.keys cinfoMap) ++ "No such canvas when CanvasConfigure in defaultEventProcess" ++ show cid 
      Just cvsInfo -> do 
        let canvas = get drawArea cvsInfo
        let page = getPage cvsInfo 
            (w,h) = get (pageDimension.viewInfo) cvsInfo
            zmode = get (zoomMode.viewInfo) cvsInfo
        cpg <- liftIO (getCanvasPageGeometry canvas page (0,0))
        let factor = getRatioFromPageToCanvas cpg zmode
            (hadj,vadj) = get adjustments cvsInfo
        liftIO $ setAdjustments (hadj,vadj) (w,h) (0,0) (0,0)
                                (w'/factor,h'/factor)
        invalidate cid
defaultEventProcess ToViewAppendMode = modeChange ToViewAppendMode
defaultEventProcess ToSelectMode = modeChange ToSelectMode 
defaultEventProcess _ = return ()



menuEventProcess :: MenuEvent -> Iteratee MyEvent XournalStateIO ()
menuEventProcess MenuQuit = liftIO $ mainQuit
menuEventProcess MenuPreviousPage = changePage (\x->x-1)
menuEventProcess MenuNextPage =  changePage (+1)
menuEventProcess MenuFirstPage = changePage (const 0)
menuEventProcess MenuLastPage = changePage (const 10000)
menuEventProcess MenuOpen = fileOpen
menuEventProcess MenuSave = fileSave 
menuEventProcess MenuSaveAs = fileSaveAs
menuEventProcess MenuCut = cutSelection
menuEventProcess MenuCopy = copySelection
menuEventProcess MenuPaste = pasteToSelection
menuEventProcess MenuDelete = deleteSelection
menuEventProcess MenuNormalSize = pageZoomChange Original  
menuEventProcess MenuPageWidth = pageZoomChange FitWidth 
menuEventProcess MenuPageHeight = pageZoomChange FitHeight
menuEventProcess MenuHSplit = eitherSplit SplitHorizontal
menuEventProcess MenuVSplit = eitherSplit SplitVertical
menuEventProcess MenuDelCanvas = deleteCanvas
menuEventProcess m = liftIO $ putStrLn $ "not implemented " ++ show m 
