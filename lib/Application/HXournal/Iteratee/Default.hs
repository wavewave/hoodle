module Application.HXournal.Iteratee.Default where

import Graphics.UI.Gtk hiding (get,set)

import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Draw
import Application.HXournal.Accessor

import Application.HXournal.Iteratee.Draw 
import Application.HXournal.Iteratee.Pen 
import Application.HXournal.Iteratee.Eraser
import Application.HXournal.Iteratee.Highlighter
import Application.HXournal.Iteratee.Scroll
import Application.HXournal.Iteratee.Page
import Application.HXournal.Iteratee.Select

import Application.HXournal.ModelAction.Adjustment
import Application.HXournal.ModelAction.Page
import Application.HXournal.Builder 

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
-- import Graphics.Xournal.Render.BBox

guiProcess :: Iteratee MyEvent XournalStateIO () 
guiProcess = do 
  initialize
  changePage (const 0)
  xstate <- lift St.get
  let cinfoMap  = get canvasInfoMap xstate
      assocs = M.toList cinfoMap 
      f (cid,cinfo) = do let canvas = get drawArea cinfo
                         (w',h') <- liftIO $ widgetGetSize canvas
                         defaultEventProcess (CanvasConfigure cid
                                                (fromIntegral w') 
                                                (fromIntegral h')) 
  mapM_ f assocs
  sequence_ (repeat dispatchMode)

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

modeChange :: MyEvent -> Iteratee MyEvent XournalStateIO ()
modeChange ToViewAppendMode = do 
  xstate <- lift St.get
  let xojstate = get xournalstate xstate
  case xojstate of 
    ViewAppendState _ -> return () 
    SelectState txoj -> do 
      liftIO $ putStrLn "to view append mode"
      lift 
        . St.put 
        . set xournalstate (ViewAppendState (XournalBBoxMap <$> tx_pages $ txoj))
        $ xstate  
--                               xournalBBoxMapFromXournalSelect xoj))

modeChange ToSelectMode = do 
  xstate <- lift St.get  
  let xojstate = get xournalstate xstate
  case xojstate of 
    ViewAppendState xoj -> do 
      liftIO $ putStrLn "to select mode"
      lift 
        . St.put 
        . set xournalstate (SelectState (tempXournalSelectFromXournalBBoxMap xoj))
        $ xstate  
 --                            (xournalSelectFromXournalBBoxMap xoj)) 
        
    SelectState _ -> return ()
modeChange _ = return ()

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
  -- liftIO $ putStrLn "selectMode"
  r1 <- await 
  case r1 of 
    PenDown cid pcoord -> do 
      ptype <- return . get (selectType.selectInfo) =<< lift St.get 
      case ptype of 
        SelectRectangleWork -> selectRectStart cid pcoord 
        _ -> return () 
    _ -> defaultEventProcess r1

defaultEventProcess :: MyEvent -> Iteratee MyEvent XournalStateIO () 
defaultEventProcess (UpdateCanvas cid) = invalidate cid   
defaultEventProcess MenuQuit = liftIO $ mainQuit
defaultEventProcess MenuPreviousPage = changePage (\x->x-1)
defaultEventProcess MenuNextPage =  changePage (+1)
defaultEventProcess MenuFirstPage = changePage (const 0)
defaultEventProcess MenuLastPage = changePage (const 10000)
defaultEventProcess MenuSave = do 
    xstate <- getSt 
    let xojstate = get xournalstate xstate
    let xoj = case xojstate of 
                 ViewAppendState xojmap -> xournalFromXournalBBoxMap xojmap 
                 SelectState txoj -> xournalFromXournalBBoxMap 
                                     $ XournalBBoxMap <$> tx_pages $ txoj 
    liftIO . L.writeFile "mytest.xoj" . builder $ xoj
defaultEventProcess MenuNormalSize = do 
    liftIO $ putStrLn "NormalSize clicked"
    xstate <- getSt 
    let currCvsId = get currentCanvas xstate
        cinfoMap = get canvasInfoMap xstate
        currCvsInfo = case M.lookup currCvsId cinfoMap of 
                        Nothing -> error " no such cvsinfo in defaultEventProcess"  
                        Just cinfo -> cinfo 
    let canvas = get drawArea currCvsInfo
    (w',h') <- liftIO $ widgetGetSize canvas
    let page = getPage currCvsInfo 
    let Dim w h = pageDim page
    let (hadj,vadj) = get adjustments currCvsInfo 
    liftIO $ setAdjustments (hadj,vadj) (w,h) (0,0) (0,0)
                           (fromIntegral w', fromIntegral h')
    let currCvsInfo' = set (zoomMode.viewInfo) Original
                       . set (viewPortOrigin.viewInfo) (0,0)
                       $ currCvsInfo 
        xstate' = updateCanvasInfo currCvsInfo' xstate
    putSt xstate' 
    invalidate currCvsId       
defaultEventProcess MenuPageWidth = do 
    liftIO $ putStrLn "PageWidth clicked"
    xstate <- getSt
    let currCvsId = get currentCanvas xstate
        cinfoMap = get canvasInfoMap xstate
        maybeCurrCvs = M.lookup currCvsId cinfoMap 
    case maybeCurrCvs of 
      Nothing -> return ()
      Just currCvsInfo -> do 
        let canvas = get drawArea currCvsInfo
        let page = getPage currCvsInfo
        let Dim w h = pageDim page 
        cpg <- liftIO (getCanvasPageGeometry canvas page (0,0))
        let (w',h') = canvas_size cpg 
        let (hadj,vadj) = get adjustments currCvsInfo 
            s = 1.0 / getRatioFromPageToCanvas cpg FitWidth 
        liftIO $ do 
          adjustmentSetUpper hadj w 
          adjustmentSetUpper vadj h 
          adjustmentSetValue hadj 0 
          adjustmentSetValue vadj 0 
          adjustmentSetPageSize hadj (w'*s)
          adjustmentSetPageSize vadj (h'*s)
        let currCvsInfo' = set (zoomMode.viewInfo) FitWidth          
                         . set (viewPortOrigin.viewInfo) (0,0) 
                         $ currCvsInfo
            xstate' =  updateCanvasInfo currCvsInfo' xstate 
        lift . St.put $ xstate'             
        invalidate currCvsId    
defaultEventProcess (HScrollBarMoved cid v) = do 
    xstate <- lift St.get 
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
    xstate <- lift St.get 
    let cinfoMap = get canvasInfoMap xstate
        cvsInfo = case M.lookup cid cinfoMap of 
                    Nothing -> error "No such canvas in defaultEventProcess"
                    Just cvs -> cvs
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


       {- liftIO $ do 
          adjustmentSetUpper hadj w 
          adjustmentSetUpper vadj h 
          adjustmentSetValue hadj 0 
          adjustmentSetValue vadj 0 
          adjustmentSetPageSize hadj (fromIntegral w')
          adjustmentSetPageSize vadj (fromIntegral h') -}
   {- liftIO $ do 
      adjustmentSetUpper hadj w 
      adjustmentSetUpper vadj h 
      adjustmentSetLower hadj 0
      adjustmentSetLower vadj 0
      adjustmentSetValue hadj 0
      adjustmentSetValue vadj 0 
      adjustmentSetPageSize hadj (w'/factor)
      adjustmentSetPageSize vadj (h'/factor) -}
      {-
                   case get currentPage currCvsInfo of 
                     Right pgselect -> mkPageBBoxMapFromPageBBox 
                                       . pageBBoxFromPageSelect $ pgselect
                     -- Right _ -> error "not implemented in defaultEventProcess"
                     Left pg -> pg
      -}
                        
                        {-           case get currentPage currCvsInfo of
                     Right pgselect -> mkPageBBoxMapFromPageBBox 
                                       . pageBBoxFromPageSelect $ pgselect
              
                     -- Right _ -> error "not implemented in defaultEventProcess"
                     Left pg -> pg -}
{-  case get currentPage cvsInfo of
                 -- Right _ -> error "not implemented in defaultEventProcess"
                 Right pageselect -> mkPageBBoxMapFromPageBBox
                                     . pageBBoxFromPageSelect $ pageselect
                 Left pg -> pg  -}
{-                . xournalFromXournalBBoxMap
                $ xoj
      SelectState xojselect -> do 
        
        liftIO $ L.writeFile "mytest.xoj" . builder 
                . xournalFromXournalBBoxMap
                $ xoj
-}        
          
    --      maybeCurrCvs = M.lookup currCvsId cinfoMap 
    --    case maybeCurrCvs of 
    --   Nothing -> return ()
    --   Just currCvsInfo -> do 
