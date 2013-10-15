{-# LANGUAGE GADTs, Rank2Types, TypeFamilies, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.View.Draw 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.View.Draw where

import Control.Applicative 
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
-- import           Control.Category ((.))
import           Control.Lens (view,set,at)
import Control.Monad (when)
import Data.Foldable hiding (elem)
import qualified Data.IntMap as M
import           Data.Maybe hiding (fromMaybe)
import           Data.Monoid
import           Data.Sequence
import           Graphics.UI.Gtk hiding (get,set)
import           Graphics.Rendering.Cairo
import           Graphics.Rendering.Pango.Cairo
-- from hoodle-platform 
import Data.Hoodle.BBox
import Data.Hoodle.Generic
import Data.Hoodle.Predefined
import Data.Hoodle.Select
import Data.Hoodle.Simple (Dimension(..),Stroke(..))
import Data.Hoodle.Zipper (currIndex,current)
import Graphics.Hoodle.Render.Generic
import Graphics.Hoodle.Render.Highlight
import Graphics.Hoodle.Render.Type
import Graphics.Hoodle.Render.Type.HitTest 
import Graphics.Hoodle.Render.Util 
-- from this package
import Hoodle.Type.Alias 
import Hoodle.Type.Canvas
import Hoodle.Type.PageArrangement
import Hoodle.Type.Enum
import Hoodle.Type.Predefined
import Hoodle.Type.Widget
import Hoodle.Util
import Hoodle.View.Coordinate
-- 
import Prelude hiding (mapM_,concatMap,foldr)

                       
-- | 
type family DrawingFunction v :: * -> * 

-- |

newtype SinglePageDraw a = 
  SinglePageDraw { unSinglePageDraw :: Bool 
                                       -> (DrawingArea, Maybe Surface) 
                                       -> (PageNum, Page a) 
                                       -> ViewInfo SinglePage 
                                       -> Maybe BBox 
                                       -> DrawFlag
                                       -> IO (Page a) }

-- | 

newtype ContPageDraw a = 
  ContPageDraw 
  { unContPageDraw :: Bool
                      -> CanvasInfo ContinuousPage 
                      -> Maybe BBox 
                      -> Hoodle a 
                      -> DrawFlag
                      -> IO (Hoodle a) }
                    
-- | 
type instance DrawingFunction SinglePage = SinglePageDraw

-- | 
type instance DrawingFunction ContinuousPage = ContPageDraw

-- | 
getCanvasViewPort :: CanvasGeometry -> ViewPortBBox 
getCanvasViewPort geometry = 
  let DeskCoord (x0,y0) = canvas2Desktop geometry (CvsCoord (0,0)) 
      CanvasDimension (Dim w h) = canvasDim geometry  
      DeskCoord (x1,y1) = canvas2Desktop geometry (CvsCoord (w,h))
  in ViewPortBBox (BBox (x0,y0) (x1,y1))

-- | 
getBBoxInPageCoord :: CanvasGeometry -> PageNum -> BBox -> BBox  
getBBoxInPageCoord geometry pnum bbox = 
  let DeskCoord (x0,y0) = page2Desktop geometry (pnum,PageCoord (0,0))  
  in moveBBoxByOffset (-x0,-y0) bbox
     
-- | 
getViewableBBox :: CanvasGeometry 
                   -> Maybe BBox   -- ^ in desktop coordinate 
                   -> IntersectBBox
getViewableBBox geometry mbbox = 
  let ViewPortBBox vportbbox = getCanvasViewPort geometry  
  in (fromMaybe mbbox :: IntersectBBox) `mappend` (Intersect (Middle vportbbox))
               
-- | double buffering within two image surfaces 
virtualDoubleBufferDraw :: (MonadIO m) => 
                           Surface  -- source surface 
                        -> Surface  -- target surface 
                        -> Render () -- pre-render before source paint 
                        -> Render () -- post-render after source paint 
                        -> m ()
virtualDoubleBufferDraw srcsfc tgtsfc pre post = 
    renderWith tgtsfc $ do 
      pre
      setSourceSurface srcsfc 0 0 
      setOperator OperatorSource 
      paint
      setOperator OperatorOver
      post  
          
-- | 
doubleBufferFlush :: ViewMode a => Surface -> CanvasInfo a -> IO () 
doubleBufferFlush sfc cinfo = do 
      let canvas = view drawArea cinfo 
      win <- widgetGetDrawWindow canvas
      renderWithDrawable win $ do 
        setSourceSurface sfc 0 0 
        setOperator OperatorSource 
        paint
  


-- | common routine for double buffering 
doubleBufferDraw :: (DrawWindow, Maybe Surface)  
                    -> CanvasGeometry -> Render () -> Render a
                    -> IntersectBBox
                    -> IO (Maybe a)
doubleBufferDraw (win,msfc) geometry _xform rndr (Intersect ibbox) = do 
  let Dim cw ch = unCanvasDimension . canvasDim $ geometry 
      mbbox' = case ibbox of 
        Top -> Just (BBox (0,0) (cw,ch))
        Middle bbox -> Just (xformBBox (unCvsCoord . desktop2Canvas geometry . DeskCoord) bbox)
        Bottom -> Nothing 
  let action = do 
        case msfc of 
	  Nothing -> do 
	    renderWithDrawable win $ do 
	      clipBBox mbbox'
              setSourceRGBA 0.5 0.5 0.5 1
              rectangle 0 0 cw ch 
              fill 
	      rndr 
	  Just sfc -> do 
	    r <- renderWith sfc $ do 
	      -- clipBBox (fmap (flip inflate (-1.0)) mbbox') -- temporary
              clipBBox mbbox' 
              setSourceRGBA 0.5 0.5 0.5 1
              rectangle 0 0 cw ch 
              fill
              clipBBox mbbox' 
	      rndr 
	    renderWithDrawable win $ do 
	      setSourceSurface sfc 0 0   
	      setOperator OperatorSource 
	      paint 
            return r 
  case ibbox of
    Top      -> Just <$> action 
    Middle _ -> Just <$> action 
    Bottom   -> return Nothing 

-- | 
cairoXform4PageCoordinate :: CanvasGeometry -> PageNum -> Render () 
cairoXform4PageCoordinate geometry pnum = do 
  let CvsCoord (x0,y0) = desktop2Canvas geometry . page2Desktop geometry $ (pnum,PageCoord (0,0))
      CvsCoord (x1,y1) = desktop2Canvas geometry . page2Desktop geometry $ (pnum,PageCoord (1,1))
      sx = x1-x0 
      sy = y1-y0
  translate x0 y0      
  scale sx sy
  
-- |   
data PressureMode = NoPressure | Pressure
  
-- | 
drawCurvebitGen  :: PressureMode 
                    -> DrawingArea
                    -> CanvasGeometry 
                    -> Double 
                    -> (Double,Double,Double,Double) 
                    -> PageNum 
                    -> Seq (Double,Double,Double)
                    -> ((Double,Double),Double) 
                    -> ((Double,Double),Double) 
                    -> IO () 
drawCurvebitGen pmode canvas geometry wdth (r,g,b,a) pnum pdraw ((x0,y0),z0) ((x,y),z) = do 
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win $ do
    cairoXform4PageCoordinate geometry pnum 
    setSourceRGBA r g b a
    case pmode of 
      NoPressure -> do 
        setLineWidth wdth
        case viewl pdraw of 
          EmptyL -> return ()
          (xo,yo,_) :< rest -> do 
            moveTo xo yo
            mapM_ (\(x',y',_)-> lineTo x' y') rest 
            lineTo x y
            stroke
        -- moveTo x0 y0
        -- lineTo x y
      Pressure -> do 
        let wx0 = 0.5*(fst predefinedPenShapeAspectXY)*wdth*z0
            wy0 = 0.5*(snd predefinedPenShapeAspectXY)*wdth*z0
            wx = 0.5*(fst predefinedPenShapeAspectXY)*wdth*z
            wy = 0.5*(snd predefinedPenShapeAspectXY)*wdth*z
        moveTo (x0-wx0) (y0-wy0)
        lineTo (x0+wx0) (y0+wy0)
        lineTo (x+wx) (y+wy)
        lineTo (x-wx) (y-wy)
        fill

-- | 
drawFuncGen :: em 
               -> ((PageNum,Page em) -> Maybe BBox -> DrawFlag -> Render (Page em)) 
               -> DrawingFunction SinglePage em
drawFuncGen _typ render = SinglePageDraw func 
  where func isCurrentCvs (canvas,msfc) (pnum,page) vinfo mbbox flag = do 
          let arr = view pageArrangement vinfo
          geometry <- makeCanvasGeometry pnum arr canvas
          win <- widgetGetDrawWindow canvas
          let ibboxnew = getViewableBBox geometry mbbox 
          let mbboxnew = toMaybe ibboxnew 
              xformfunc = cairoXform4PageCoordinate geometry pnum
              renderfunc = do
                xformfunc 
                pg <- render (pnum,page) mbboxnew flag
                -- Start Widget
                when isCurrentCvs (emphasisCanvasRender ColorBlue geometry)  
                -- End Widget
                resetClip 
                return pg 
          doubleBufferDraw (win,msfc) geometry xformfunc renderfunc ibboxnew 
          >>= maybe (return page) return 


-- | 
drawFuncSelGen :: ((PageNum,Page SelectMode) -> Maybe BBox -> DrawFlag -> Render ()) 
                  -> ((PageNum,Page SelectMode) -> Maybe BBox -> DrawFlag -> Render ())
                  -> DrawingFunction SinglePage SelectMode  
drawFuncSelGen rencont rensel = drawFuncGen SelectMode (\x y f -> rencont x y f >> rensel x y f >> return (snd x)) 

-- |
emphasisCanvasRender :: PenColor -> CanvasGeometry -> Render ()
emphasisCanvasRender pcolor geometry = do 
  identityMatrix
  let CanvasDimension (Dim cw ch) = canvasDim geometry 
  let (r,g,b,a) = convertPenColorToRGBA pcolor
  setSourceRGBA r g b a 
  setLineWidth 2
  rectangle 0 0 cw ch 
  stroke

-- | highlight current page
emphasisPageRender :: CanvasGeometry -> (PageNum,Page EditMode) -> Render ()
emphasisPageRender geometry (pn,pg) = do 
    save
    identityMatrix 
    cairoXform4PageCoordinate geometry pn 
    let Dim w h = view gdimension pg 
    setSourceRGBA 0 0 1.0 1 
    setLineWidth 2 
    rectangle 0 0 w h 
    stroke
    restore 

-- | highlight notified item (like link)
emphasisNotifiedRender :: CanvasGeometry -> (PageNum,BBox,RItem) -> Render ()
emphasisNotifiedRender geometry (pn,BBox (x1,y1) (x2,y2),_) = do 
    save
    identityMatrix 
    cairoXform4PageCoordinate geometry pn 
    setSourceRGBA 1.0 1.0 0 0.1 
    rectangle x1 y1 (x2-x1) (y2-y1)
    fill 
    restore 



-- |
drawContPageGen :: ((PageNum,Page EditMode) -> Maybe BBox -> DrawFlag -> Render (Int,Page EditMode)) 
                   -> DrawingFunction ContinuousPage EditMode
drawContPageGen render = ContPageDraw func 
  where func :: Bool -> CanvasInfo ContinuousPage ->Maybe BBox -> Hoodle EditMode -> DrawFlag -> IO (Hoodle EditMode)
        func isCurrentCvs cinfo mbbox hdl flag = do 
          let arr = view (viewInfo.pageArrangement) cinfo
              pnum = PageNum . view currentPageNum $ cinfo 
              canvas = view drawArea cinfo 
              msfc = view mDrawSurface cinfo
          geometry <- makeCanvasGeometry pnum arr canvas
          let pgs = view gpages hdl 
              mcpg = view (at (unPageNum pnum)) pgs 
          let drawpgs = catMaybes . map f 
                        $ (getPagesInViewPortRange geometry hdl) 
                where f k = maybe Nothing (\a->Just (k,a)) 
                            . M.lookup (unPageNum k) $ pgs
          win <- widgetGetDrawWindow canvas
          let ibboxnew = getViewableBBox geometry mbbox 
          let mbboxnew = toMaybe ibboxnew 
              xformfunc = cairoXform4PageCoordinate geometry pnum
              onepagerender (pn,pg) = do  
                identityMatrix 
                cairoXform4PageCoordinate geometry pn
                let pgmbbox = fmap (getBBoxInPageCoord geometry pn) mbboxnew
                render (pn,pg) pgmbbox flag
              renderfunc = do
                xformfunc 
                ndrawpgs <- mapM onepagerender drawpgs 
                let npgs = foldr rfunc pgs ndrawpgs   
                       where rfunc (k,pg) m = M.adjust (const pg) k m 
                let nhdl = set gpages npgs hdl  
                mapM_ (\cpg->emphasisPageRender geometry (pnum,cpg)) mcpg 
                mapM_ (emphasisNotifiedRender geometry) (view notifiedItem cinfo) 
                when isCurrentCvs (emphasisCanvasRender ColorRed geometry)
                let mbbox_canvas = fmap (xformBBox (unCvsCoord . desktop2Canvas geometry . DeskCoord )) mbboxnew                 
                drawWidgets allWidgets hdl cinfo mbbox_canvas 
                resetClip 
                return nhdl 
          doubleBufferDraw (win,msfc) geometry xformfunc renderfunc ibboxnew
          >>= maybe (return hdl) return  


-- |
drawContPageSelGen :: ((PageNum,Page EditMode) -> Maybe BBox -> DrawFlag -> Render (Int,Page EditMode)) 
                      -> ((PageNum, Page SelectMode) -> Maybe BBox -> DrawFlag -> Render (Int,Page SelectMode))
                      -> DrawingFunction ContinuousPage SelectMode
drawContPageSelGen rendergen rendersel = ContPageDraw func 
  where func :: Bool -> CanvasInfo ContinuousPage ->Maybe BBox -> Hoodle SelectMode ->DrawFlag -> IO (Hoodle SelectMode) 
        func isCurrentCvs cinfo mbbox thdl flag = do 
          let arr = view (viewInfo.pageArrangement) cinfo
              pnum = PageNum . view currentPageNum $ cinfo 
              mtpage = view gselSelected thdl 
              canvas = view drawArea cinfo 
              msfc = view mDrawSurface cinfo 
              pgs = view gselAll thdl 
              mcpg = view (at (unPageNum pnum)) pgs 
              hdl = gSelect2GHoodle thdl  
          geometry <- makeCanvasGeometry pnum arr canvas
          let drawpgs = catMaybes . map f 
                        $ (getPagesInViewPortRange geometry hdl) 
                where f k = maybe Nothing (\a->Just (k,a)) 
                            . M.lookup (unPageNum k) $ pgs
          win <- widgetGetDrawWindow canvas
          let ibboxnew = getViewableBBox geometry mbbox
              mbboxnew = toMaybe ibboxnew
              xformfunc = cairoXform4PageCoordinate geometry pnum
              onepagerender (pn,pg) = do  
                identityMatrix 
                cairoXform4PageCoordinate geometry pn
                rendergen (pn,pg) (fmap (getBBoxInPageCoord geometry pn) mbboxnew) flag
              selpagerender :: (PageNum, Page SelectMode) -> Render (Int, Page SelectMode) 
              selpagerender (pn,pg) = do 
                identityMatrix 
                cairoXform4PageCoordinate geometry pn
                rendersel (pn,pg) (fmap (getBBoxInPageCoord geometry pn) mbboxnew) flag
              renderfunc :: Render (Hoodle SelectMode)
              renderfunc = do
                xformfunc 
                ndrawpgs <- mapM onepagerender drawpgs 
                let npgs = foldr rfunc pgs ndrawpgs   
                       where rfunc (k,pg) m = M.adjust (const pg) k m 
                let nthdl :: Hoodle SelectMode 
                    nthdl = set gselAll npgs thdl  
                r <- runMaybeT $ do (n,tpage) <- MaybeT (return mtpage)
                                    lift (selpagerender (PageNum n,tpage)) 
                let nthdl2 = set gselSelected r nthdl
                maybe (return ()) (\cpg->emphasisPageRender geometry (pnum,cpg)) mcpg 
                mapM_ (emphasisNotifiedRender geometry) (view notifiedItem cinfo)                 
                when isCurrentCvs (emphasisCanvasRender ColorGreen geometry)  
                let mbbox_canvas = fmap (xformBBox (unCvsCoord . desktop2Canvas geometry . DeskCoord )) mbboxnew                 
                drawWidgets allWidgets hdl cinfo mbbox_canvas
                resetClip 
                return nthdl2  
          doubleBufferDraw (win,msfc) geometry xformfunc renderfunc ibboxnew
          >>= maybe (return thdl) return 


-- |
drawSinglePage :: DrawingFunction SinglePage EditMode
drawSinglePage = drawFuncGen EditMode f 
  where f (_,page) _ Clear = do 
          pg' <- cairoRenderOption (RBkgDrawPDF,DrawFull) page 
          return pg' 
        f(_,page) mbbox BkgEfficient = do 
          InBBoxBkgBuf pg' <- cairoRenderOption (InBBoxOption mbbox) (InBBoxBkgBuf page) 
          return pg' 
        f (_,page) mbbox Efficient = do 
          InBBox pg' <- cairoRenderOption (InBBoxOption mbbox) (InBBox page) 
          return pg' 

-- |
drawSinglePageSel :: CanvasGeometry -> DrawingFunction SinglePage SelectMode    
drawSinglePageSel geometry = drawFuncSelGen rendercontent renderselect
  where rendercontent (_pnum,tpg) mbbox flag = do
          let pg' = hPage2RPage tpg 
          case flag of 
            Clear -> cairoRenderOption (RBkgDrawPDF,DrawFull) pg' >> return ()
            BkgEfficient -> cairoRenderOption (InBBoxOption mbbox) (InBBoxBkgBuf pg') >> return ()            
            Efficient -> cairoRenderOption (InBBoxOption mbbox) (InBBox pg') >> return ()
          return ()
        renderselect (_pnum,tpg) mbbox _flag = do 
          cairoHittedBoxDraw geometry tpg mbbox
          return ()

-- | 
drawContHoodle :: DrawingFunction ContinuousPage EditMode
drawContHoodle = drawContPageGen f  
  where f (PageNum n,page) _ Clear = (,) n <$> cairoRenderOption (RBkgDrawPDF,DrawFull) page 
        f (PageNum n,page) mbbox BkgEfficient = (,) n . unInBBoxBkgBuf <$> cairoRenderOption (InBBoxOption mbbox) (InBBoxBkgBuf page)                  
        f (PageNum n,page) mbbox Efficient = (,) n . unInBBox <$> cairoRenderOption (InBBoxOption mbbox) (InBBox page)


-- |
drawContHoodleSel :: CanvasGeometry -> DrawingFunction ContinuousPage SelectMode
drawContHoodleSel geometry = drawContPageSelGen renderother renderselect 
  where renderother (PageNum n,page) mbbox flag = do
          case flag of 
            Clear -> (,) n <$> cairoRenderOption (RBkgDrawPDF,DrawFull) page 
            BkgEfficient -> (,) n . unInBBoxBkgBuf <$> cairoRenderOption (InBBoxOption mbbox) (InBBoxBkgBuf page)            
            Efficient -> (,) n . unInBBox <$> cairoRenderOption (InBBoxOption mbbox) (InBBox page)
        renderselect (PageNum n,tpg) mbbox _flag = do
          cairoHittedBoxDraw geometry tpg mbbox 
          return (n,tpg)

-- |
cairoHittedBoxDraw :: CanvasGeometry->Page SelectMode -> Maybe BBox -> Render () 
cairoHittedBoxDraw geometry tpg mbbox = do   
  let layers = view glayers tpg 
      slayer = view selectedLayer layers 
  case unTEitherAlterHitted . view gitems $ slayer of
    Right alist -> do 
      clipBBox mbbox
      setSourceRGBA 0.0 0.0 1.0 1.0
      let hititms = concatMap unHitted (getB alist)
      mapM_ renderSelectedItem hititms 
      let ulbbox = unUnion . mconcat . fmap (Union .Middle . getBBox) 
                   $ hititms
      case ulbbox of 
        Middle bbox -> renderSelectHandle geometry bbox 
        _ -> return () 
      resetClip
    Left _ -> return ()  

-- | 
renderLasso :: CanvasGeometry -> Seq (Double,Double) -> Render ()
renderLasso geometry lst = do 
  let z = canvas2DesktopRatio geometry
  setLineWidth (predefinedLassoWidth*z)
  uncurry4 setSourceRGBA predefinedLassoColor
  let (dasha,dashb) = predefinedLassoDash 
      adjusteddash = (fmap (*z) dasha,dashb*z) 
  uncurry setDash adjusteddash
  case viewl lst of 
    EmptyL -> return ()
    x :< xs -> do uncurry moveTo x
                  mapM_ (uncurry lineTo) xs 
                  stroke 

-- |
renderBoxSelection :: BBox -> Render () 
renderBoxSelection bbox = do
  setLineWidth predefinedLassoWidth
  uncurry4 setSourceRGBA predefinedLassoColor
  uncurry setDash predefinedLassoDash 
  let (x1,y1) = bbox_upperleft bbox
      (x2,y2) = bbox_lowerright bbox
  rectangle x1 y1 (x2-x1) (y2-y1)
  stroke

-- |
renderSelectedStroke :: BBoxed Stroke -> Render () 
renderSelectedStroke str = do 
  setLineWidth 1.5
  setSourceRGBA 0 0 1 1
  renderStrkHltd str


-- |
renderSelectedItem :: RItem -> Render () 
renderSelectedItem itm = do 
  setLineWidth 1.5
  setSourceRGBA 0 0 1 1
  renderRItemHltd itm

-- | 
canvas2DesktopRatio :: CanvasGeometry -> Double 
canvas2DesktopRatio geometry =
  let DeskCoord (tx1,_) = canvas2Desktop geometry (CvsCoord (0,0)) 
      DeskCoord (tx2,_) = canvas2Desktop geometry (CvsCoord (1,0))
  in tx2-tx1


-- |
renderSelectHandle :: CanvasGeometry -> BBox -> Render () 
renderSelectHandle geometry bbox = do 
  let z = canvas2DesktopRatio geometry 
  setLineWidth (predefinedLassoWidth*z)
  uncurry4 setSourceRGBA predefinedLassoColor
  let (dasha,dashb) = predefinedLassoDash 
      adjusteddash = (fmap (*z) dasha,dashb*z) 
  uncurry setDash adjusteddash 
  let (x1,y1) = bbox_upperleft bbox
      (x2,y2) = bbox_lowerright bbox
      hsize = predefinedLassoHandleSize*z
  rectangle x1 y1 (x2-x1) (y2-y1)
  stroke
  setSourceRGBA 1 0 0 0.8
  rectangle (x1-hsize) (y1-hsize) (2*hsize) (2*hsize)
  fill
  setSourceRGBA 1 0 0 0.8
  rectangle (x1-hsize) (y2-hsize) (2*hsize) (2*hsize)
  fill
  setSourceRGBA 1 0 0 0.8
  rectangle (x2-hsize) (y1-hsize) (2*hsize) (2*hsize)
  fill
  setSourceRGBA 1 0 0 0.8
  rectangle (x2-hsize) (y2-hsize) (2*hsize) (2*hsize)
  fill
  setSourceRGBA 0.5 0 0.2 0.8
  rectangle (x1-hsize*0.6) (0.5*(y1+y2)-hsize*0.6) (1.2*hsize) (1.2*hsize)  
  fill
  setSourceRGBA 0.5 0 0.2 0.8
  rectangle (x2-hsize*0.6) (0.5*(y1+y2)-hsize*0.6) (1.2*hsize) (1.2*hsize)
  fill
  setSourceRGBA 0.5 0 0.2 0.8
  rectangle (0.5*(x1+x2)-hsize*0.6) (y1-hsize*0.6) (1.2*hsize) (1.2*hsize)
  fill
  setSourceRGBA 0.5 0 0.2 0.8
  rectangle (0.5*(x1+x2)-hsize*0.6) (y2-hsize*0.6) (1.2*hsize) (1.2*hsize)
  fill



-- | 
canvasImageSurface :: Maybe Double  -- ^ multiply 
                   -> CanvasGeometry
                   -> Hoodle EditMode 
                   -> IO (Surface,Dimension)
canvasImageSurface mmulti geometry hdl = do 
  let ViewPortBBox bbx_desk = getCanvasViewPort geometry 
      nbbx_desk = case mmulti of
                    Nothing -> bbx_desk 
                    Just z -> let (x0,y0) = bbox_upperleft bbx_desk
                                  (x1,y1) = bbox_lowerright bbx_desk
                                  Dim ws_desk hs_desk = bboxToDim bbx_desk
                              in BBox (x0-z*ws_desk,y0-z*hs_desk) (x1+z*ws_desk,y1+z*hs_desk) 
      nbbx_cvs = 
        xformBBox ( unCvsCoord . desktop2Canvas geometry . DeskCoord ) nbbx_desk
      nvport = ViewPortBBox nbbx_desk
      Dim w_cvs  h_cvs  = bboxToDim nbbx_cvs
  let pgs = view gpages hdl 
      drawpgs = (catMaybes . map f . getPagesInRange geometry nvport) hdl 
        where f k = maybe Nothing (\a -> Just (k,a)) . M.lookup (unPageNum k) $ pgs
      onepagerender (pn,pg) = do 
        identityMatrix 
        case mmulti of 
          Nothing -> return ()
          Just z -> do 
            let (ws_cvs,hs_cvs) = (w_cvs/(2*z+1),h_cvs/(2*z+1)) 
            translate (z*ws_cvs) (z*hs_cvs)
        cairoXform4PageCoordinate geometry pn
        cairoRenderOption (InBBoxOption Nothing) (InBBox pg)
      renderfunc = do 
        setSourceRGBA 0.5 0.5 0.5 1
        rectangle 0 0 w_cvs h_cvs        
        fill 
        
        mapM_ onepagerender drawpgs 
  print (Prelude.length drawpgs)
  sfc <- createImageSurface FormatARGB32 (floor w_cvs) (floor h_cvs)
  renderWith sfc renderfunc 
  return (sfc, Dim w_cvs h_cvs)

---------------------------------------------------
--                Widgets                        --
---------------------------------------------------

-- | 
drawWidgets :: ViewMode a => 
               [WidgetItem] -> Hoodle EditMode -> CanvasInfo a -> Maybe BBox -> Render () 
drawWidgets witms hdl cinfo mbbox = do  
  when (PanZoomWidget `elem` witms && view (canvasWidgets.widgetConfig.doesUsePanZoomWidget) cinfo) $
    renderPanZoomWidget (view (canvasWidgets.panZoomWidgetConfig.panZoomWidgetTouchIsZoom) cinfo)
      mbbox (view (canvasWidgets.panZoomWidgetConfig.panZoomWidgetPosition) cinfo) 
  when (LayerWidget `elem` witms && view (canvasWidgets.widgetConfig.doesUseLayerWidget) cinfo) 
    (drawLayerWidget hdl cinfo mbbox (view (canvasWidgets.layerWidgetConfig.layerWidgetPosition) cinfo))

  when (ClockWidget `elem` witms && view (canvasWidgets.widgetConfig.doesUseClockWidget) cinfo) $
    renderClockWidget mbbox (view (canvasWidgets.clockWidgetConfig.clockWidgetPosition) cinfo)    



---------------------
-- Pan Zoom Widget --
---------------------

-- | 
renderPanZoomWidget :: Bool -> Maybe BBox -> CanvasCoordinate -> Render () 
renderPanZoomWidget b mbbox (CvsCoord (x,y)) = do 
  identityMatrix 
  clipBBox mbbox 
  setSourceRGBA 0.5 0.5 0.2 0.3 
  rectangle x y 100 100 
  fill 
  setSourceRGBA 0.2 0.2 0.7 0.5
  rectangle (x+10) (y+10) 40 80
  fill 
  setSourceRGBA 0.2 0.7 0.2 0.5 
  rectangle (x+50) (y+10) 40 80
  fill 
  setSourceRGBA 0.7 0.2 0.2 (if b then 1.0 else 0.5)
  rectangle (x+30) (y+30) 40 40 
  fill  
  setSourceRGBA 0.5 0.5 0.5 0.5
  rectangle x y 10 10
  fill
  setSourceRGBA 0 0 0 0.7 
  setLineWidth 1
  moveTo x y 
  lineTo (x+10) (y+10)
  stroke 
  moveTo x (y+10)
  lineTo (x+10) y
  stroke
  resetClip 

------------------
-- Layer Widget -- 
------------------

drawLayerWidget :: ViewMode a => 
                   Hoodle EditMode 
                   -> CanvasInfo a 
                   -> Maybe BBox 
                   -> CanvasCoordinate 
                   -> Render ()
drawLayerWidget hdl cinfo mbbox cvscoord = do     
    let cpn = view currentPageNum cinfo
        lc = view (canvasWidgets.layerWidgetConfig) cinfo 
    runMaybeT $ do 
      pg <- MaybeT . return $ view (gpages.at cpn) hdl
      let lyrs = view glayers pg
          n = currIndex lyrs 
          l = current lyrs 
          LyBuf msfc = view gbuffer l
      lift $ renderLayerWidget (show n) mbbox cvscoord 
      when (view layerWidgetShowContent lc) $ do 
        sfc <- MaybeT . return $ msfc 
        lift $ renderLayerContent mbbox (view gdimension pg) sfc cvscoord
    return () 

renderLayerContent :: Maybe BBox -> Dimension -> Surface -> CanvasCoordinate -> Render ()
renderLayerContent mbbox (Dim w h) sfc (CvsCoord (x,y)) = do 
  identityMatrix 
  clipBBox mbbox 
  let sx = 200 / w
  rectangle (x+100) y 200 (h*200/w)
  setLineWidth 0.5 
  setSourceRGBA 0 0 0 1 
  stroke 
  translate (x+100) (y) 
  scale sx sx 
  setSourceSurface sfc 0 0 
  paint 
  

-- | 
renderLayerWidget :: String -> Maybe BBox -> CanvasCoordinate -> Render () 
renderLayerWidget str mbbox (CvsCoord (x,y)) = do 
  identityMatrix 
  clipBBox mbbox 
  setSourceRGBA 0.5 0.5 0.2 0.3 
  rectangle x y 100 100 
  fill 
  rectangle x y 10 10
  fill
  setSourceRGBA 0 0 0 0.7 
  setLineWidth 1
  moveTo x y 
  lineTo (x+10) (y+10)
  stroke 
  moveTo x (y+10)
  lineTo (x+10) y
  stroke
  -- upper right
  setSourceRGBA 0 0 0 0.4
  moveTo (x+80) y 
  lineTo (x+100) y
  lineTo (x+100) (y+20)
  fill 
  -- lower left
  setSourceRGBA 0 0 0 0.1
  moveTo x (y+80)
  lineTo x (y+100)
  lineTo (x+20) (y+100)
  fill 
  -- middle right
  setSourceRGBA 0 0 0 0.3
  moveTo (x+90) (y+40)
  lineTo (x+100) (y+50)
  lineTo (x+90) (y+60)
  fill
  -- 
  identityMatrix 
  l1 <- createLayout "layer"
  updateLayout l1 
  (_,reclog) <- liftIO $ layoutGetExtents l1
  let PangoRectangle _ _ w1 h1 = reclog 
  moveTo (x+15) y
  let sx1 = 50 / w1 
      sy1 = 20 / h1 
  scale sx1 sy1 
  layoutPath l1
  setSourceRGBA 0 0 0 0.4
  fill
  -- 
  identityMatrix
  l <- createLayout str 
  updateLayout l 
  (_,reclog) <- liftIO $ layoutGetExtents l
  let PangoRectangle _ _ w h = reclog 
  moveTo (x+30) (y+20)
  let sx = 40 / w 
      sy = 60 / h 
  scale sx sy 
  layoutPath l 
  setSourceRGBA 0 0 0 0.4
  fill

------------------
-- Clock Widget --
------------------

renderClockWidget :: Maybe BBox -> CanvasCoordinate -> Render () 
renderClockWidget mbbox (CvsCoord (x,y)) = do 
  identityMatrix 
  clipBBox mbbox 
  setSourceRGBA 0.5 0.5 0.2 0.3 
  arc x y 50 0.0 (2.0*pi)
  fill 
  resetClip 

