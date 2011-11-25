{-# LANGUAGE OverloadedStrings #-}

module Application.HXournal.Iteratee where 

import Control.Applicative hiding (empty)
import Control.Monad
import qualified Control.Monad.State as St 
import Control.Monad.Trans 
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.IO.Class

import Control.Category
import Data.Label
import Prelude hiding ((.),id)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S

import Application.HXournal.Type
import Application.HXournal.Type.Event
import Application.HXournal.Util
import Application.HXournal.Draw
import Application.HXournal.Coroutine
import Application.HXournal.Builder
import Application.HXournal.Accessor

import Text.Xournal.Type 
import Text.Xournal.Predefined 

import Graphics.UI.Gtk hiding (get,set)

import Data.Maybe
import qualified Data.Map as M
import Data.Foldable (toList)
import Data.Sequence hiding (length,drop,take)
import Data.Strict.Tuple hiding (uncurry,fst,snd)

import Application.HXournal.Device

guiProcess :: Iteratee MyEvent XournalStateIO () 
guiProcess = do initialize
                changePage (const 0)
                canvas <- get drawArea <$> lift St.get   
                (w',h') <- liftIO $ widgetGetSize canvas
                defaultEventProcess (CanvasConfigure (fromIntegral w') 
                                                     (fromIntegral h')) 
                sequence_ (repeat eventProcess)
                return ()

connPenMove :: (WidgetClass w) => w -> Iteratee MyEvent XournalStateIO (ConnectId w) 
connPenMove c = do 
  callbk <- get callBack <$> lift St.get 
  dev <- get deviceList <$> lift St.get 
  liftIO (c `on` motionNotifyEvent $ tryEvent $ do 
             p <- getPointer dev
             liftIO (callbk (PenMove p)))

connPenUp :: (WidgetClass w) => w -> Iteratee MyEvent XournalStateIO (ConnectId w) 
connPenUp c = do 
  callbk <- get callBack <$> lift St.get 
  dev <- get deviceList <$> lift St.get 
  liftIO (c `on` buttonReleaseEvent $ tryEvent $ do 
             p <- getPointer dev
             liftIO (callbk (PenMove p)))


initialize :: Iteratee MyEvent XournalStateIO ()
initialize = do ev <- await 
                liftIO $ putStrLn $ show ev 
                case ev of 
                  Initialized -> return () 
                  _ -> initialize

changePage :: (Int -> Int) -> Iteratee MyEvent XournalStateIO () 
changePage modifyfn = do 
  xstate <- lift St.get 
  let xoj = get xournal xstate 
      totalnumofpages = length . xoj_pages $ xoj
      oldpage = get currentPageNum xstate
  let newpage | modifyfn oldpage >= totalnumofpages = totalnumofpages - 1
              | modifyfn oldpage < 0  = 0 
              | otherwise = modifyfn oldpage 
      Dim w h = page_dim . (!! newpage) . xoj_pages $ xoj 
      hadj = get horizAdjustment xstate  
      vadj = get vertAdjustment xstate
  liftIO $ do 
    adjustmentSetUpper hadj w 
    adjustmentSetUpper vadj h 
    adjustmentSetValue hadj 0
    adjustmentSetValue vadj 0
  
  let xstate' = set (viewPortOrigin.viewInfo) (0,0) 
              . set (pageDim.viewInfo) (w,h) 
              . set currentPageNum newpage
              $ xstate 
  lift . St.put $ xstate' 
  invalidate   

invalidate :: Iteratee MyEvent XournalStateIO () 
invalidate = do 
  xstate <- lift St.get  
  liftIO (updateCanvas <$> get drawArea 
                       <*> get xournal 
                       <*> get currentPageNum 
                       <*> get viewInfo 
                       $ xstate )


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

penStart :: PointerCoord -> Iteratee MyEvent XournalStateIO ()
penStart pcoord = do 
    xstate <- lift St.get 
    let canvas = get drawArea xstate   
    win <- liftIO $ widgetGetDrawWindow canvas
    let pagenum = get currentPageNum xstate 
        page = (!!pagenum) . xoj_pages . get xournal $  xstate 
        (x0,y0) = get (viewPortOrigin.viewInfo) xstate 
        currxoj = get xournal xstate
        pinfo = get penInfo xstate
        zmode = get (zoomMode.viewInfo) xstate 
    geometry <- liftIO (getCanvasPageGeometry canvas page (x0,y0) )
    let (x,y) = device2pageCoord geometry zmode pcoord 
    connidup <- connPenUp canvas      
    connidmove <- connPenMove canvas
    pdraw <- penProcess geometry connidmove connidup (empty |> (x,y)) (x,y) 
    let newxoj = addPDraw pinfo currxoj pagenum pdraw
    lift . St.put . set xournal newxoj $ xstate 
    return ()

penProcess :: CanvasPageGeometry
           -> ConnectId DrawingArea -> ConnectId DrawingArea 
           -> Seq (Double,Double) -> (Double,Double) 
           -> Iteratee MyEvent XournalStateIO (Seq (Double,Double))
penProcess cpg connidmove connidup pdraw (x0,y0) = do 
  r <- await 
  xstate <- lift St.get
  case r of 
    PenMove pcoord -> do 
      let canvas = get drawArea xstate 
          zmode  = get (zoomMode.viewInfo) xstate
          pcolor = get (penColor.penInfo) xstate 
          pwidth = get (penWidth.penInfo) xstate 
      let (x,y) = device2pageCoord cpg zmode pcoord 
          pcolRGBA = fromJust (M.lookup pcolor penColorRGBAmap) 
      liftIO $ drawSegment canvas cpg zmode pwidth pcolRGBA (x0,y0) (x,y)
      penProcess cpg connidmove connidup (pdraw |> (x,y)) (x,y) 
    PenUp pcoord -> do 
      let canvas = get drawArea xstate 
          zmode = get (zoomMode.viewInfo) xstate 
      let (x,y) = device2pageCoord cpg zmode pcoord 
      liftIO $ signalDisconnect connidmove
      liftIO $ signalDisconnect connidup
      return (pdraw |> (x,y)) 
    other -> do
      defaultEventProcess other        
      penProcess cpg connidmove connidup pdraw (x0,y0) 

eraserStart :: PointerCoord -> Iteratee MyEvent XournalStateIO ()
eraserStart pcoord = do 
  liftIO $ putStrLn "eraser started"


highlighterStart :: PointerCoord -> Iteratee MyEvent XournalStateIO ()
highlighterStart pcoord = do 
  liftIO $ putStrLn "highlighter started"


defaultEventProcess :: MyEvent -> Iteratee MyEvent XournalStateIO () 
defaultEventProcess UpdateCanvas = invalidate   
defaultEventProcess MenuPreviousPage = changePage (\x->x-1)
defaultEventProcess MenuNextPage =  changePage (+1)
defaultEventProcess MenuFirstPage = changePage (const 0)
defaultEventProcess MenuLastPage = changePage (const 10000)
defaultEventProcess MenuSave = do 
    xojcontent <- get xournal <$> lift St.get   
    liftIO $ L.writeFile "mytest.xoj" $ builder xojcontent
defaultEventProcess MenuNormalSize = do 
    liftIO $ putStrLn "NormalSize clicked"
    xstate <- lift St.get 
    let canvas = get drawArea xstate 
    (w',h') <- liftIO $ widgetGetSize canvas
    let cpn = get currentPageNum xstate 
    let Dim w h = page_dim . (!! cpn) . xoj_pages . get xournal $ xstate     
    let hadj = get horizAdjustment xstate
        vadj = get vertAdjustment xstate
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
        page = (!! cpn) . xoj_pages . get xournal $ xstate
        Dim w h = page_dim page 
    cpg <- liftIO (getCanvasPageGeometry canvas page (0,0))
    let (w',h') = canvas_size cpg 
    let hadj = get horizAdjustment xstate
        vadj = get vertAdjustment xstate
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
        page = (!! cpn) . xoj_pages . get xournal $ xstate        
        (w,h) = get (pageDim.viewInfo) xstate 
        zmode = get (zoomMode.viewInfo) xstate 
    cpg <- liftIO (getCanvasPageGeometry  canvas page (0,0))
    let factor = getRatioFromPageToCanvas cpg zmode
        hadj = get horizAdjustment xstate
        vadj = get vertAdjustment xstate
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
  

addPDraw :: PenInfo -> Xournal -> Int -> Seq (Double,Double) -> Xournal
addPDraw pinfo xoj pgnum pdraw = 
  let pcolor = get penColor pinfo
      pcolname = fromJust (M.lookup pcolor penColorNameMap)
      pwidth = get penWidth pinfo
      pagesbefore = take pgnum $ xoj_pages xoj  
      pagesafter  = drop (pgnum+1) $ xoj_pages xoj
      currpage = ((!!pgnum).xoj_pages) xoj 
      currlayer = head (page_layers currpage)
      otherlayers = tail (page_layers currpage)
      newstroke = Stroke { stroke_tool = "pen" 
                         , stroke_color = pcolname 
                         , stroke_width = pwidth
                         , stroke_data = map (uncurry (:!:)) . toList $ pdraw
                         } 
      newlayer = currlayer {layer_strokes = layer_strokes currlayer ++ [newstroke]}
      newpage = currpage {page_layers = newlayer : otherlayers }
      newxoj = xoj { xoj_pages =  pagesbefore ++ [newpage] ++ pagesafter }  
  in  newxoj

