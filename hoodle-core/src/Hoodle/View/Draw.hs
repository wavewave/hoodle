{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-} 
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.View.Draw 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
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
import           Control.Lens (view,set,at)
import Control.Monad (when)
import Data.Foldable hiding (elem)
import qualified Data.IntMap as M
import           Data.Maybe hiding (fromMaybe)
import           Data.Monoid
import           Data.Sequence
import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.UI.Gtk hiding (get,set)
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
type family DrawingFunction (v :: ViewMode) :: * -> * 

-- |

newtype SinglePageDraw a = 
  SinglePageDraw 
  { unSinglePageDraw :: RenderCache 
                     -> Bool                               -- ^ isCurrentCanvas
                     -> (DrawingArea, Maybe Cairo.Surface) 
                     -> (PageNum, Page a) 
                     -> ViewInfo SinglePage 
                     -> Maybe BBox 
                     -> DrawFlag
                     -> IO (Page a) }

-- | 

newtype ContPageDraw a = 
  ContPageDraw 
  { unContPageDraw :: RenderCache
                   -> Bool                                 -- ^ isCurrentCanvas 
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
                           Cairo.Surface  -- source surface 
                        -> Cairo.Surface  -- target surface 
                        -> Cairo.Render () -- pre-render before source paint 
                        -> Cairo.Render () -- post-render after source paint 
                        -> m ()
virtualDoubleBufferDraw srcsfc tgtsfc pre post = 
    Cairo.renderWith tgtsfc $ do 
      pre
      Cairo.setSourceSurface srcsfc 0 0 
      Cairo.setOperator Cairo.OperatorSource 
      Cairo.paint
      Cairo.setOperator Cairo.OperatorOver
      post  
          
-- | 
doubleBufferFlush :: Cairo.Surface -> CanvasInfo a -> IO () 
doubleBufferFlush sfc cinfo = do 
      let canvas = view drawArea cinfo 
      win <- widgetGetDrawWindow canvas
      renderWithDrawable win $ do 
        Cairo.setSourceSurface sfc 0 0 
        Cairo.setOperator Cairo.OperatorSource 
        Cairo.paint
  


-- | common routine for double buffering 
doubleBufferDraw :: (DrawWindow, Maybe Cairo.Surface)  
                    -> CanvasGeometry -> Cairo.Render () -> Cairo.Render a
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
              Cairo.setSourceRGBA 0.5 0.5 0.5 1
              Cairo.rectangle 0 0 cw ch 
              Cairo.fill 
	      rndr 
	  Just sfc -> do 
	    r <- Cairo.renderWith sfc $ do 
              clipBBox mbbox' 
              Cairo.setSourceRGBA 0.5 0.5 0.5 1
              Cairo.rectangle 0 0 cw ch 
              Cairo.fill
              clipBBox mbbox' 
	      rndr 
	    renderWithDrawable win $ do 
	      Cairo.setSourceSurface sfc 0 0   
	      Cairo.setOperator Cairo.OperatorSource 
	      Cairo.paint 
            return r 
  case ibbox of
    Top      -> Just <$> action 
    Middle _ -> Just <$> action 
    Bottom   -> return Nothing 

-- | 
cairoXform4PageCoordinate :: CanvasGeometry -> PageNum -> Cairo.Render () 
cairoXform4PageCoordinate geometry pnum = do 
  let CvsCoord (x0,y0) = desktop2Canvas geometry . page2Desktop geometry $ (pnum,PageCoord (0,0))
      CvsCoord (x1,y1) = desktop2Canvas geometry . page2Desktop geometry $ (pnum,PageCoord (1,1))
      sx = x1-x0 
      sy = y1-y0
  Cairo.translate x0 y0      
  Cairo.scale sx sy
  
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
    Cairo.setSourceRGBA r g b a
    case pmode of 
      NoPressure -> do 
        Cairo.setLineWidth wdth
        case viewl pdraw of 
          EmptyL -> return ()
          (xo,yo,_) :< rest -> do 
            Cairo.moveTo xo yo
            mapM_ (\(x',y',_)-> Cairo.lineTo x' y') rest 
            Cairo.lineTo x y
            Cairo.stroke
      Pressure -> do 
        let wx0 = 0.5*(fst predefinedPenShapeAspectXY)*wdth*z0
            wy0 = 0.5*(snd predefinedPenShapeAspectXY)*wdth*z0
            wx = 0.5*(fst predefinedPenShapeAspectXY)*wdth*z
            wy = 0.5*(snd predefinedPenShapeAspectXY)*wdth*z
        Cairo.moveTo (x0-wx0) (y0-wy0)
        Cairo.lineTo (x0+wx0) (y0+wy0)
        Cairo.lineTo (x+wx) (y+wy)
        Cairo.lineTo (x-wx) (y-wy)
        Cairo.fill

-- | 
drawFuncGen :: em 
               -> (RenderCache -> (PageNum,Page em) -> Maybe BBox 
                   -> DrawFlag -> Cairo.Render (Page em)) 
               -> DrawingFunction SinglePage em
drawFuncGen _typ render = SinglePageDraw func 
  where func cache isCurrentCvs (canvas,msfc) (pnum,page) vinfo mbbox flag = do 
          let arr = view pageArrangement vinfo
          geometry <- makeCanvasGeometry pnum arr canvas
          win <- widgetGetDrawWindow canvas
          let ibboxnew = getViewableBBox geometry mbbox 
          let mbboxnew = toMaybe ibboxnew 
              xformfunc = cairoXform4PageCoordinate geometry pnum
              renderfunc = do
                xformfunc 
                pg <- render cache (pnum,page) mbboxnew flag
                -- Start Widget
                when isCurrentCvs (emphasisCanvasRender ColorBlue geometry)  
                -- End Widget
                Cairo.resetClip 
                return pg 
          doubleBufferDraw (win,msfc) geometry xformfunc renderfunc ibboxnew 
          >>= maybe (return page) return 


-- | 
drawFuncSelGen :: (RenderCache -> (PageNum,Page SelectMode) -> Maybe BBox 
                   -> DrawFlag -> Cairo.Render ()) 
                  -> (RenderCache -> (PageNum,Page SelectMode) -> Maybe BBox 
                      -> DrawFlag -> Cairo.Render ())
                  -> DrawingFunction SinglePage SelectMode  
drawFuncSelGen rencont rensel = drawFuncGen SelectMode (\c x y f -> rencont c x y f >> rensel c x y f >> return (snd x)) 

-- |
emphasisCanvasRender :: PenColor -> CanvasGeometry -> Cairo.Render ()
emphasisCanvasRender pcolor geometry = do 
  Cairo.identityMatrix
  let CanvasDimension (Dim cw ch) = canvasDim geometry 
  let (r,g,b,a) = convertPenColorToRGBA pcolor
  Cairo.setSourceRGBA r g b a 
  Cairo.setLineWidth 2
  Cairo.rectangle 0 0 cw ch 
  Cairo.stroke

-- | highlight current page
emphasisPageRender :: CanvasGeometry -> (PageNum,Page EditMode) -> Cairo.Render ()
emphasisPageRender geometry (pn,pg) = do 
    Cairo.save
    Cairo.identityMatrix 
    cairoXform4PageCoordinate geometry pn 
    let Dim w h = view gdimension pg 
    Cairo.setSourceRGBA 0 0 1.0 1 
    Cairo.setLineWidth 2 
    Cairo.rectangle 0 0 w h 
    Cairo.stroke
    Cairo.restore 

-- | highlight notified item (like link)
emphasisNotifiedRender :: CanvasGeometry -> (PageNum,BBox,RItem) -> Cairo.Render ()
emphasisNotifiedRender geometry (pn,BBox (x1,y1) (x2,y2),_) = do 
    Cairo.save
    Cairo.identityMatrix 
    cairoXform4PageCoordinate geometry pn 
    Cairo.setSourceRGBA 1.0 1.0 0 0.1 
    Cairo.rectangle x1 y1 (x2-x1) (y2-y1)
    Cairo.fill 
    Cairo.restore 



-- |
drawContPageGen :: (RenderCache -> (PageNum,Page EditMode) -> Maybe BBox 
                    -> DrawFlag -> Cairo.Render (Int,Page EditMode)) 
                   -> DrawingFunction ContinuousPage EditMode
drawContPageGen render = ContPageDraw func 
  where func :: RenderCache -> Bool -> CanvasInfo ContinuousPage 
             -> Maybe BBox -> Hoodle EditMode -> DrawFlag -> IO (Hoodle EditMode)
        func cache isCurrentCvs cinfo mbbox hdl flag = do 
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
                Cairo.identityMatrix 
                cairoXform4PageCoordinate geometry pn
                let pgmbbox = fmap (getBBoxInPageCoord geometry pn) mbboxnew
                render cache (pn,pg) pgmbbox flag
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
                Cairo.resetClip 
                return nhdl 
          doubleBufferDraw (win,msfc) geometry xformfunc renderfunc ibboxnew
          >>= maybe (return hdl) return  


-- |
drawContPageSelGen :: (RenderCache -> (PageNum,Page EditMode) -> Maybe BBox 
                       -> DrawFlag -> Cairo.Render (Int,Page EditMode)) 
                   -> (RenderCache -> (PageNum, Page SelectMode) -> Maybe BBox 
                       -> DrawFlag -> Cairo.Render (Int,Page SelectMode))
                   -> DrawingFunction ContinuousPage SelectMode
drawContPageSelGen rendergen rendersel = ContPageDraw func 
  where func :: RenderCache -> Bool -> CanvasInfo ContinuousPage 
             -> Maybe BBox -> Hoodle SelectMode ->DrawFlag -> IO (Hoodle SelectMode) 
        func cache isCurrentCvs cinfo mbbox thdl flag = do 
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
                Cairo.identityMatrix 
                cairoXform4PageCoordinate geometry pn
                rendergen cache (pn,pg) (fmap (getBBoxInPageCoord geometry pn) mbboxnew) flag
              selpagerender :: (PageNum, Page SelectMode) 
                            -> Cairo.Render (Int, Page SelectMode) 
              selpagerender (pn,pg) = do 
                Cairo.identityMatrix 
                cairoXform4PageCoordinate geometry pn
                rendersel cache (pn,pg) (fmap (getBBoxInPageCoord geometry pn) mbboxnew) flag
              renderfunc :: Cairo.Render (Hoodle SelectMode)
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
                Cairo.resetClip 
                return nthdl2  
          doubleBufferDraw (win,msfc) geometry xformfunc renderfunc ibboxnew
          >>= maybe (return thdl) return 


-- |
drawSinglePage :: DrawingFunction SinglePage EditMode
drawSinglePage = drawFuncGen EditMode f 
  where f cache (_,page) _ Clear = do 
          pg' <- cairoRenderOption (RBkgDrawPDF,DrawFull) cache page 
          return pg' 
        f cache (_,page) mbbox BkgEfficient = do 
          InBBoxBkgBuf pg' <- cairoRenderOption (InBBoxOption mbbox) cache (InBBoxBkgBuf page) 
          return pg' 
        f cache (_,page) mbbox Efficient = do 
          InBBox pg' <- cairoRenderOption (InBBoxOption mbbox) cache (InBBox page) 
          return pg' 

-- |
drawSinglePageSel :: CanvasGeometry -> DrawingFunction SinglePage SelectMode    
drawSinglePageSel geometry = drawFuncSelGen rendercontent renderselect
  where rendercontent cache (_pnum,tpg) mbbox flag = do
          let pg' = hPage2RPage tpg 
          case flag of 
            Clear -> cairoRenderOption (RBkgDrawPDF,DrawFull) cache pg' >> return ()
            BkgEfficient -> cairoRenderOption (InBBoxOption mbbox) cache (InBBoxBkgBuf pg') >> return ()            
            Efficient -> cairoRenderOption (InBBoxOption mbbox) cache (InBBox pg') >> return ()
          return ()
        renderselect _cache (_pnum,tpg) mbbox _flag = do 
          cairoHittedBoxDraw geometry tpg mbbox
          return ()

-- | 
drawContHoodle :: DrawingFunction ContinuousPage EditMode
drawContHoodle = drawContPageGen f  
  where f cache (PageNum n,page) _ Clear = (,) n <$> cairoRenderOption (RBkgDrawPDF,DrawFull) cache page 
        f cache (PageNum n,page) mbbox BkgEfficient = (,) n . unInBBoxBkgBuf <$> cairoRenderOption (InBBoxOption mbbox) cache (InBBoxBkgBuf page)                  
        f cache (PageNum n,page) mbbox Efficient = (,) n . unInBBox <$> cairoRenderOption (InBBoxOption mbbox) cache (InBBox page)


-- |
drawContHoodleSel :: CanvasGeometry 
                  -> DrawingFunction ContinuousPage SelectMode
drawContHoodleSel geometry = drawContPageSelGen renderother renderselect 
  where renderother cache (PageNum n,page) mbbox flag = do
          case flag of 
            Clear -> (,) n <$> cairoRenderOption (RBkgDrawPDF,DrawFull) cache page 
            BkgEfficient -> (,) n . unInBBoxBkgBuf <$> cairoRenderOption (InBBoxOption mbbox) cache (InBBoxBkgBuf page)            
            Efficient -> (,) n . unInBBox <$> cairoRenderOption (InBBoxOption mbbox) cache (InBBox page)
        renderselect _cache (PageNum n,tpg) mbbox _flag = do
          cairoHittedBoxDraw geometry tpg mbbox 
          return (n,tpg)

-- |
cairoHittedBoxDraw :: CanvasGeometry
                   -> Page SelectMode 
                   -> Maybe BBox 
                   -> Cairo.Render () 
cairoHittedBoxDraw geometry tpg mbbox = do   
  let layers = view glayers tpg 
      slayer = view selectedLayer layers 
  case unTEitherAlterHitted . view gitems $ slayer of
    Right alist -> do 
      clipBBox mbbox
      Cairo.setSourceRGBA 0.0 0.0 1.0 1.0
      let hititms = concatMap unHitted (getB alist)
      mapM_ renderSelectedItem hititms 
      let ulbbox = unUnion . mconcat . fmap (Union .Middle . getBBox) 
                   $ hititms
      case ulbbox of 
        Middle bbox -> renderSelectHandle geometry bbox 
        _ -> return () 
      Cairo.resetClip
    Left _ -> return ()  

-- | 
renderLasso :: CanvasGeometry -> Seq (Double,Double) -> Cairo.Render ()
renderLasso geometry lst = do 
  let z = canvas2DesktopRatio geometry
  Cairo.setLineWidth (predefinedLassoWidth*z)
  uncurry4 Cairo.setSourceRGBA predefinedLassoColor
  let (dasha,dashb) = predefinedLassoDash 
      adjusteddash = (fmap (*z) dasha,dashb*z) 
  uncurry Cairo.setDash adjusteddash
  case viewl lst of 
    EmptyL -> return ()
    x :< xs -> do uncurry Cairo.moveTo x
                  mapM_ (uncurry Cairo.lineTo) xs 
                  Cairo.stroke 

-- |
renderBoxSelection :: BBox -> Cairo.Render () 
renderBoxSelection bbox = do
  Cairo.setLineWidth predefinedLassoWidth
  uncurry4 Cairo.setSourceRGBA predefinedLassoColor
  uncurry Cairo.setDash predefinedLassoDash 
  let (x1,y1) = bbox_upperleft bbox
      (x2,y2) = bbox_lowerright bbox
  Cairo.rectangle x1 y1 (x2-x1) (y2-y1)
  Cairo.stroke

-- |
renderSelectedStroke :: BBoxed Stroke -> Cairo.Render () 
renderSelectedStroke str = do 
  Cairo.setLineWidth 1.5
  Cairo.setSourceRGBA 0 0 1 1
  renderStrkHltd str


-- |
renderSelectedItem :: RItem -> Cairo.Render () 
renderSelectedItem itm = do 
  Cairo.setLineWidth 1.5
  Cairo.setSourceRGBA 0 0 1 1
  renderRItemHltd itm

-- | 
canvas2DesktopRatio :: CanvasGeometry -> Double 
canvas2DesktopRatio geometry =
  let DeskCoord (tx1,_) = canvas2Desktop geometry (CvsCoord (0,0)) 
      DeskCoord (tx2,_) = canvas2Desktop geometry (CvsCoord (1,0))
  in tx2-tx1

-- |
renderSelectHandle :: CanvasGeometry -> BBox -> Cairo.Render () 
renderSelectHandle geometry bbox = do 
  let z = canvas2DesktopRatio geometry 
  Cairo.setLineWidth (predefinedLassoWidth*z)
  uncurry4 Cairo.setSourceRGBA predefinedLassoColor
  let (dasha,dashb) = predefinedLassoDash 
      adjusteddash = (fmap (*z) dasha,dashb*z) 
  uncurry Cairo.setDash adjusteddash 
  let (x1,y1) = bbox_upperleft bbox
      (x2,y2) = bbox_lowerright bbox
      hsize = predefinedLassoHandleSize*z
  Cairo.rectangle x1 y1 (x2-x1) (y2-y1)
  Cairo.stroke
  Cairo.setSourceRGBA 1 0 0 0.8
  Cairo.rectangle (x1-hsize) (y1-hsize) (2*hsize) (2*hsize)
  Cairo.fill
  Cairo.setSourceRGBA 1 0 0 0.8
  Cairo.rectangle (x1-hsize) (y2-hsize) (2*hsize) (2*hsize)
  Cairo.fill
  Cairo.setSourceRGBA 1 0 0 0.8
  Cairo.rectangle (x2-hsize) (y1-hsize) (2*hsize) (2*hsize)
  Cairo.fill
  Cairo.setSourceRGBA 1 0 0 0.8
  Cairo.rectangle (x2-hsize) (y2-hsize) (2*hsize) (2*hsize)
  Cairo.fill
  Cairo.setSourceRGBA 0.5 0 0.2 0.8
  Cairo.rectangle (x1-hsize*0.6) (0.5*(y1+y2)-hsize*0.6) (1.2*hsize) (1.2*hsize)  
  Cairo.fill
  Cairo.setSourceRGBA 0.5 0 0.2 0.8
  Cairo.rectangle (x2-hsize*0.6) (0.5*(y1+y2)-hsize*0.6) (1.2*hsize) (1.2*hsize)
  Cairo.fill
  Cairo.setSourceRGBA 0.5 0 0.2 0.8
  Cairo.rectangle (0.5*(x1+x2)-hsize*0.6) (y1-hsize*0.6) (1.2*hsize) (1.2*hsize)
  Cairo.fill
  Cairo.setSourceRGBA 0.5 0 0.2 0.8
  Cairo.rectangle (0.5*(x1+x2)-hsize*0.6) (y2-hsize*0.6) (1.2*hsize) (1.2*hsize)
  Cairo.fill

-- | 
canvasImageSurface :: RenderCache
                   -> Maybe Double  -- ^ multiply 
                   -> CanvasGeometry
                   -> Hoodle EditMode 
                   -> IO (Cairo.Surface,Dimension)
canvasImageSurface cache mmulti geometry hdl = do 
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
        Cairo.identityMatrix 
        case mmulti of 
          Nothing -> return ()
          Just z -> do 
            let (ws_cvs,hs_cvs) = (w_cvs/(2*z+1),h_cvs/(2*z+1)) 
            Cairo.translate (z*ws_cvs) (z*hs_cvs)
        cairoXform4PageCoordinate geometry pn
        cairoRenderOption (InBBoxOption Nothing) cache (InBBox pg)
      renderfunc = do 
        Cairo.setSourceRGBA 0.5 0.5 0.5 1
        Cairo.rectangle 0 0 w_cvs h_cvs        
        Cairo.fill 
        mapM_ onepagerender drawpgs 
  print (Prelude.length drawpgs)
  sfc <- Cairo.createImageSurface Cairo.FormatARGB32 (floor w_cvs) (floor h_cvs)
  Cairo.renderWith sfc renderfunc 
  return (sfc, Dim w_cvs h_cvs)

---------------------------------------------------
--                Widgets                        --
---------------------------------------------------

-- | 
drawWidgets :: [WidgetItem] 
            -> Hoodle EditMode 
            -> CanvasInfo a 
            -> Maybe BBox
            -> Cairo.Render () 
drawWidgets witms hdl cinfo mbbox = do  
  when (PanZoomWidget `elem` witms && view (canvasWidgets.widgetConfig.doesUsePanZoomWidget) cinfo) $
    renderPanZoomWidget (view (canvasWidgets.panZoomWidgetConfig.panZoomWidgetTouchIsZoom) cinfo)
      mbbox (view (canvasWidgets.panZoomWidgetConfig.panZoomWidgetPosition) cinfo) 
  when (LayerWidget `elem` witms && view (canvasWidgets.widgetConfig.doesUseLayerWidget) cinfo) 
    (drawLayerWidget hdl cinfo mbbox (view (canvasWidgets.layerWidgetConfig.layerWidgetPosition) cinfo))

  when (ClockWidget `elem` witms && view (canvasWidgets.widgetConfig.doesUseClockWidget) cinfo) $
    renderClockWidget mbbox (view (canvasWidgets.clockWidgetConfig) cinfo)    



---------------------
-- Pan Zoom Widget --
---------------------

-- | 
renderPanZoomWidget :: Bool -> Maybe BBox -> CanvasCoordinate -> Cairo.Render () 
renderPanZoomWidget b mbbox (CvsCoord (x,y)) = do 
    Cairo.identityMatrix 
    clipBBox mbbox 
    Cairo.setSourceRGBA 0.5 0.5 0.2 0.3 
    Cairo.rectangle x y 100 100 
    Cairo.fill 
    Cairo.setSourceRGBA 0.2 0.2 0.7 0.5
    Cairo.rectangle (x+10) (y+10) 40 80
    Cairo.fill 
    Cairo.setSourceRGBA 0.2 0.7 0.2 0.5 
    Cairo.rectangle (x+50) (y+10) 40 80
    Cairo.fill 
    Cairo.setSourceRGBA 0.7 0.2 0.2 (if b then 1.0 else 0.5)
    Cairo.rectangle (x+30) (y+30) 40 40 
    Cairo.fill  
    Cairo.setSourceRGBA 0.5 0.5 0.5 0.5
    Cairo.rectangle x y 10 10
    Cairo.fill
    Cairo.setSourceRGBA 0 0 0 0.7 
    Cairo.setLineWidth 1
    Cairo.moveTo x y 
    Cairo.lineTo (x+10) (y+10)
    Cairo.stroke 
    Cairo.moveTo x (y+10)
    Cairo.lineTo (x+10) y
    Cairo.stroke
    Cairo.resetClip 

------------------
-- Layer Widget -- 
------------------

drawLayerWidget :: Hoodle EditMode 
                -> CanvasInfo a 
                -> Maybe BBox 
                -> CanvasCoordinate 
                -> Cairo.Render ()
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

renderLayerContent :: Maybe BBox 
                   -> Dimension 
                   -> Cairo.Surface
                   -> CanvasCoordinate 
                   -> Cairo.Render ()
renderLayerContent mbbox (Dim w h) sfc (CvsCoord (x,y)) = do 
  Cairo.identityMatrix 
  clipBBox mbbox 
  let sx = 200 / w
  Cairo.rectangle (x+100) y 200 (h*200/w)
  Cairo.setLineWidth 0.5 
  Cairo.setSourceRGBA 0 0 0 1 
  Cairo.stroke 
  Cairo.translate (x+100) (y) 
  Cairo.scale sx sx 
  Cairo.setSourceSurface sfc 0 0 
  Cairo.paint 
  

-- | 
renderLayerWidget :: String -> Maybe BBox -> CanvasCoordinate -> Cairo.Render () 
renderLayerWidget str mbbox (CvsCoord (x,y)) = do 
  Cairo.identityMatrix 
  clipBBox mbbox 
  Cairo.setSourceRGBA 0.5 0.5 0.2 0.3 
  Cairo.rectangle x y 100 100 
  Cairo.fill 
  Cairo.rectangle x y 10 10
  Cairo.fill
  Cairo.setSourceRGBA 0 0 0 0.7 
  Cairo.setLineWidth 1
  Cairo.moveTo x y 
  Cairo.lineTo (x+10) (y+10)
  Cairo.stroke 
  Cairo.moveTo x (y+10)
  Cairo.lineTo (x+10) y
  Cairo.stroke
  -- upper right
  Cairo.setSourceRGBA 0 0 0 0.4
  Cairo.moveTo (x+80) y 
  Cairo.lineTo (x+100) y
  Cairo.lineTo (x+100) (y+20)
  Cairo.fill 
  -- lower left
  Cairo.setSourceRGBA 0 0 0 0.1
  Cairo.moveTo x (y+80)
  Cairo.lineTo x (y+100)
  Cairo.lineTo (x+20) (y+100)
  Cairo.fill 
  -- middle right
  Cairo.setSourceRGBA 0 0 0 0.3
  Cairo.moveTo (x+90) (y+40)
  Cairo.lineTo (x+100) (y+50)
  Cairo.lineTo (x+90) (y+60)
  Cairo.fill
  -- 
  Cairo.identityMatrix 
  l1 <- createLayout "layer"
  updateLayout l1 
  (_,reclog) <- liftIO $ layoutGetExtents l1
  let PangoRectangle _ _ w1 h1 = reclog 
  Cairo.moveTo (x+15) y
  let sx1 = 50 / w1 
      sy1 = 20 / h1 
  Cairo.scale sx1 sy1 
  layoutPath l1
  Cairo.setSourceRGBA 0 0 0 0.4
  Cairo.fill
  -- 
  Cairo.identityMatrix
  l <- createLayout str 
  updateLayout l 
  (_,reclog2) <- liftIO $ layoutGetExtents l
  let PangoRectangle _ _ w h = reclog2 
  Cairo.moveTo (x+30) (y+20)
  let sx = 40 / w 
      sy = 60 / h 
  Cairo.scale sx sy 
  layoutPath l 
  Cairo.setSourceRGBA 0 0 0 0.4
  Cairo.fill

------------------
-- Clock Widget --
------------------

renderClockWidget :: Maybe BBox -> ClockWidgetConfig -> Cairo.Render () 
renderClockWidget mbbox cfg = do 
  let CvsCoord (x,y) = view clockWidgetPosition cfg 
      (h,m,s) = view clockWidgetTime cfg
      div2rad :: Int -> Int -> Double
      div2rad n theta = fromIntegral theta/fromIntegral n * 2.0*pi  
  Cairo.identityMatrix 
  clipBBox mbbox 
  Cairo.setSourceRGBA 0.5 0.5 0.2 0.3 
  Cairo.arc x y 50 0.0 (2.0*pi)
  Cairo.fill 
  --
  Cairo.setSourceRGBA 1 0 0 0.7
  Cairo.setLineWidth 0.5
  Cairo.moveTo x y 
  Cairo.lineTo (x+45*sin (div2rad 60 s)) (y-45*cos (div2rad 60 s))
  Cairo.stroke
  --
  Cairo.setSourceRGBA 0 0 0 1
  Cairo.setLineWidth 1.0
  Cairo.moveTo x y 
  Cairo.lineTo (x+50*sin (div2rad 60 m)) (y-50*cos (div2rad 60 m)) 
  Cairo.stroke
  -- 
  Cairo.setSourceRGBA 0 0 0 1
  Cairo.setLineWidth 2.0
  Cairo.moveTo x y 
  Cairo.lineTo (x+30*sin (div2rad 12 h + div2rad 720 m)) 
               (y-30*cos (div2rad 12 h + div2rad 720 m)) 
  Cairo.stroke
  -- 
  Cairo.resetClip 

