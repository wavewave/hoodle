module Application.HXournal.Iteratee.Default where

import Graphics.UI.Gtk

import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
-- import Application.HXournal.Draw
import Application.HXournal.Accessor

import Application.HXournal.Iteratee.Draw 

import qualified Control.Monad.State as St 

import Control.Monad.Trans

import qualified Data.Map as M
import Data.Maybe

import Text.Xournal.Type 



guiProcess :: Iteratee MyEvent XournalStateIO () 
guiProcess = do initialize
                changePage (const 0)
                canvas <- get drawArea <$> lift St.get   
                (w',h') <- liftIO $ widgetGetSize canvas
                defaultEventProcess (CanvasConfigure (fromIntegral w') 
                                                     (fromIntegral h')) 
                sequence_ (repeat eventProcess)
                return ()

initialize :: Iteratee MyEvent XournalStateIO ()
initialize = do ev <- await 
                liftIO $ putStrLn $ show ev 
                case ev of 
                  Initialized -> return () 
                  _ -> initialize

changePage :: (Int -> Int) -> Iteratee MyEvent XournalStateIO () 
changePage modifyfn = do 
  xstate <- lift St.get 
  let xoj = get xournalbbox xstate 
      totalnumofpages = length . xournalPages $ xoj
      oldpage = get currentPageNum xstate
  let newpage | modifyfn oldpage >= totalnumofpages = totalnumofpages - 1
              | modifyfn oldpage < 0  = 0 
              | otherwise = modifyfn oldpage 
      Dim w h = pageDim . (!! newpage) . xournalPages $ xoj 
      (hadj,vadj) = get adjustments xstate 
  liftIO $ do 
    adjustmentSetUpper hadj w 
    adjustmentSetUpper vadj h 
    adjustmentSetValue hadj 0
    adjustmentSetValue vadj 0
  
  let xstate' = set (viewPortOrigin.viewInfo) (0,0) 
              . set (pageDimension.viewInfo) (w,h) 
              . set currentPageNum newpage
              $ xstate 
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
    let canvas = get drawArea xstate 
    (w',h') <- liftIO $ widgetGetSize canvas
    let cpn = get currentPageNum xstate 
    let Dim w h = pageDim . (!! cpn) . xournalPages . get xournalbbox $ xstate
    let (hadj,vadj) = get adjustments xstate 
    liftIO $ do 
      adjustmentSetUpper hadj w 
      adjustmentSetUpper vadj h 
      adjustmentSetValue hadj 0 
      adjustmentSetValue vadj 0 
      adjustmentSetPageSize hadj (fromIntegral w')
      adjustmentSetPageSize vadj (fromIntegral h')
    lift . St.put . set (zoomMode.viewInfo) Original
                  . set (viewPortOrigin.viewInfo) (0,0)
                  $ xstate
    invalidate       
defaultEventProcess MenuPageWidth = do 
    liftIO $ putStrLn "PageWidth clicked"
    xstate <- lift St.get 
    let canvas = get drawArea xstate 
        cpn = get currentPageNum xstate
        page = (!! cpn) . xournalPages . get xournalbbox $ xstate
        Dim w h = pageDim page 
    cpg <- liftIO (getCanvasPageGeometry canvas page (0,0))
    let (w',h') = canvas_size cpg 
    let (hadj,vadj) = get adjustments xstate 
        s = 1.0 / getRatioFromPageToCanvas cpg FitWidth 
    liftIO $ do 
      adjustmentSetUpper hadj w 
      adjustmentSetUpper vadj h 
      adjustmentSetValue hadj 0 
      adjustmentSetValue vadj 0 
      adjustmentSetPageSize hadj (w'*s)
      adjustmentSetPageSize vadj (h'*s)
    lift . St.put . set (zoomMode.viewInfo) FitWidth          
                  . set (viewPortOrigin.viewInfo) (0,0) 
                  $ xstate 
    invalidate       
defaultEventProcess (HScrollBarMoved v) = do 
    xstate <- lift St.get 
    let vm_orig = get (viewPortOrigin.viewInfo) xstate 
    lift . St.put . set (viewPortOrigin.viewInfo) (v,snd vm_orig) $ xstate 
    invalidate
defaultEventProcess (VScrollBarMoved v) = do 
    xstate <- lift St.get 
    let vm_orig = get (viewPortOrigin.viewInfo) xstate 
    lift . St.put . set (viewPortOrigin.viewInfo) (fst vm_orig,v) $ xstate 
    invalidate
defaultEventProcess (CanvasConfigure w' h') = do 
    xstate <- lift St.get 
    let canvas = get drawArea xstate
        cpn = get currentPageNum xstate 
        page = (!! cpn) . xournalPages . get xournalbbox $ xstate        
        (w,h) = get (pageDimension.viewInfo) xstate 
        zmode = get (zoomMode.viewInfo) xstate 
    cpg <- liftIO (getCanvasPageGeometry  canvas page (0,0))
    let factor = getRatioFromPageToCanvas cpg zmode
        (hadj,vadj) = get adjustments xstate 
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
