module Application.HXournal.Iteratee.Default where

import Graphics.UI.Gtk hiding (get,set)

import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.XournalBBox
import Application.HXournal.Draw
import Application.HXournal.Accessor

import Application.HXournal.Iteratee.Draw 
import Application.HXournal.Iteratee.Pen 
import Application.HXournal.Iteratee.Eraser
import Application.HXournal.Iteratee.Highlighter

import Application.HXournal.Builder 

import Control.Applicative 
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import qualified Control.Monad.State as St 

import Control.Monad.Trans

import qualified Data.Map as M
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import Control.Category
import Data.Label
import Prelude hiding ((.), id)

import Text.Xournal.Type 


guiProcess :: Iteratee MyEvent XournalStateIO () 
guiProcess = do 
  initialize
  changePage (const 0)
  xstate <- lift St.get
  let currCvsId = get currentCanvas xstate
      maybeCurrCvs = M.lookup currCvsId (get canvasInfoMap xstate)
  case maybeCurrCvs of 
    Nothing -> return ()
    Just currCvsInfo -> do 
      let canvas = get drawArea currCvsInfo
      (w',h') <- liftIO $ widgetGetSize canvas
      defaultEventProcess (CanvasConfigure (fromIntegral w') 
                                           (fromIntegral h')) 
      sequence_ (repeat eventProcess)

initialize :: Iteratee MyEvent XournalStateIO ()
initialize = do ev <- await 
                liftIO $ putStrLn $ show ev 
                case ev of 
                  Initialized -> return () 
                  _ -> initialize

changePage :: (Int -> Int) -> Iteratee MyEvent XournalStateIO () 
changePage modifyfn = do 
  xstate <- lift St.get 
  let currCvsId = get currentCanvas xstate
      cinfoMap = get canvasInfoMap xstate
      maybeCurrCvs = M.lookup currCvsId cinfoMap 
  case maybeCurrCvs of 
    Nothing -> return ()
    Just currCvsInfo -> do 
      let xoj = get xournalbbox xstate 
          pages = xournalPages xoj 
          totalnumofpages = length pages
          oldpage = get currentPageNum currCvsInfo
      let newpage | modifyfn oldpage >= totalnumofpages = totalnumofpages - 1
                  | modifyfn oldpage < 0  = 0 
                  | otherwise = modifyfn oldpage 
          Dim w h = pageDim . (!! newpage) $ pages
          (hadj,vadj) = get adjustments currCvsInfo
      liftIO $ do 
        adjustmentSetUpper hadj w 
        adjustmentSetUpper vadj h 
        adjustmentSetValue hadj 0
        adjustmentSetValue vadj 0
  
      let currCvsInfo' = set (viewPortOrigin.viewInfo) (0,0) 
                       . set (pageDimension.viewInfo) (w,h) 
                       . set currentPageNum newpage
                       $ currCvsInfo
          cinfoMap' = M.adjust (\_ -> currCvsInfo') currCvsId cinfoMap  
          xstate' = set canvasInfoMap cinfoMap' xstate
      lift . St.put $ xstate' 
      invalidate   

eventProcess :: Iteratee MyEvent XournalStateIO ()
eventProcess = do 
  r1 <- await 
  case r1 of 
    PenDown pcoord -> do 
      ptype <- getPenType 
      case ptype of 
        PenWork         -> penStart pcoord 
        EraserWork      -> eraserStart pcoord 
        HighlighterWork -> highlighterStart pcoord 
    _ -> defaultEventProcess r1

defaultEventProcess :: MyEvent -> Iteratee MyEvent XournalStateIO () 
defaultEventProcess UpdateCanvas = invalidate   
defaultEventProcess MenuQuit = liftIO $ mainQuit
defaultEventProcess MenuPreviousPage = changePage (\x->x-1)
defaultEventProcess MenuNextPage =  changePage (+1)
defaultEventProcess MenuFirstPage = changePage (const 0)
defaultEventProcess MenuLastPage = changePage (const 10000)
defaultEventProcess MenuSave = do 
    xojcontent <- get xournalbbox <$> lift St.get   
    liftIO $ L.writeFile "mytest.xoj" . builder . xournalFromXournalBBox 
           $ xojcontent
defaultEventProcess MenuNormalSize = do 
    liftIO $ putStrLn "NormalSize clicked"
    xstate <- lift St.get 
    let currCvsId = get currentCanvas xstate
        cinfoMap = get canvasInfoMap xstate
        maybeCurrCvs = M.lookup currCvsId cinfoMap 
    case maybeCurrCvs of 
      Nothing -> return ()
      Just currCvsInfo -> do 
        let canvas = get drawArea currCvsInfo
        (w',h') <- liftIO $ widgetGetSize canvas
        let cpn = get currentPageNum currCvsInfo
        let Dim w h = pageDim . (!! cpn) . xournalPages . get xournalbbox $ xstate
        let (hadj,vadj) = get adjustments currCvsInfo 
        liftIO $ do 
          adjustmentSetUpper hadj w 
          adjustmentSetUpper vadj h 
          adjustmentSetValue hadj 0 
          adjustmentSetValue vadj 0 
          adjustmentSetPageSize hadj (fromIntegral w')
          adjustmentSetPageSize vadj (fromIntegral h')
        let currCvsInfo' = set (zoomMode.viewInfo) Original
                         . set (viewPortOrigin.viewInfo) (0,0)
                         $ currCvsInfo 
            cinfoMap' = M.adjust (\_ -> currCvsInfo') currCvsId cinfoMap  
            xstate' = set canvasInfoMap cinfoMap' xstate
        lift . St.put $ xstate' 
        invalidate       
defaultEventProcess MenuPageWidth = do 
    liftIO $ putStrLn "PageWidth clicked"
    xstate <- lift St.get 
    let currCvsId = get currentCanvas xstate
        cinfoMap = get canvasInfoMap xstate
        maybeCurrCvs = M.lookup currCvsId cinfoMap 
    case maybeCurrCvs of 
      Nothing -> return ()
      Just currCvsInfo -> do 
        let canvas = get drawArea currCvsInfo
            cpn = get currentPageNum currCvsInfo
            page = (!! cpn) . xournalPages . get xournalbbox $ xstate
            Dim w h = pageDim page 
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
            cinfoMap' = M.adjust (\_ -> currCvsInfo') currCvsId cinfoMap  
            xstate' = set canvasInfoMap cinfoMap' xstate
        lift . St.put $ xstate'             
        invalidate       
defaultEventProcess (HScrollBarMoved v) = do 
    xstate <- lift St.get 
    let currCvsId = get currentCanvas xstate
        cinfoMap = get canvasInfoMap xstate
        maybeCurrCvs = M.lookup currCvsId cinfoMap 
    case maybeCurrCvs of 
      Nothing -> return ()
      Just currCvsInfo -> do 
        let vm_orig = get (viewPortOrigin.viewInfo) currCvsInfo
        let currCvsInfo' = set (viewPortOrigin.viewInfo) (v,snd vm_orig) 
                         $ currCvsInfo
            cinfoMap' = M.adjust (\_ -> currCvsInfo') currCvsId cinfoMap  
            xstate' = set canvasInfoMap cinfoMap' xstate
        lift . St.put $ xstate'
        invalidate
defaultEventProcess (VScrollBarMoved v) = do 
    xstate <- lift St.get 
    let currCvsId = get currentCanvas xstate
        cinfoMap = get canvasInfoMap xstate
        maybeCurrCvs = M.lookup currCvsId cinfoMap 
    case maybeCurrCvs of 
      Nothing -> return ()
      Just currCvsInfo -> do 
        let vm_orig = get (viewPortOrigin.viewInfo) currCvsInfo
        let currCvsInfo' = set (viewPortOrigin.viewInfo) (fst vm_orig,v)
                         $ currCvsInfo 
            cinfoMap' = M.adjust (\_ -> currCvsInfo') currCvsId cinfoMap  
            xstate' = set canvasInfoMap cinfoMap' xstate
        lift . St.put $ xstate'
        invalidate
defaultEventProcess (CanvasConfigure w' h') = do 
    xstate <- lift St.get 
    let currCvsId = get currentCanvas xstate
        cinfoMap = get canvasInfoMap xstate
        maybeCurrCvs = M.lookup currCvsId cinfoMap 
    case maybeCurrCvs of 
      Nothing -> return ()
      Just currCvsInfo -> do 
        let canvas = get drawArea currCvsInfo
            cpn = get currentPageNum currCvsInfo
            page = (!! cpn) . xournalPages . get xournalbbox $ xstate        
            (w,h) = get (pageDimension.viewInfo) currCvsInfo
            zmode = get (zoomMode.viewInfo) currCvsInfo
        cpg <- liftIO (getCanvasPageGeometry canvas page (0,0))
        let factor = getRatioFromPageToCanvas cpg zmode
            (hadj,vadj) = get adjustments currCvsInfo
        liftIO $ do 
          adjustmentSetUpper hadj w 
          adjustmentSetUpper vadj h 
          adjustmentSetLower hadj 0
          adjustmentSetLower vadj 0
          adjustmentSetValue hadj 0
          adjustmentSetValue vadj 0 
          adjustmentSetPageSize hadj (w'/factor)
          adjustmentSetPageSize vadj (h'/factor)
        invalidate 
        return () 
defaultEventProcess _ = return ()
