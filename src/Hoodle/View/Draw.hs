{-# LANGUAGE GADTs, Rank2Types, TypeFamilies, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.View.Draw 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.View.Draw where

import Control.Applicative 
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Graphics.UI.Gtk hiding (get,set)
import Graphics.Rendering.Cairo
import Control.Category ((.))
import           Control.Lens (view,set,at)
import Control.Monad (when)
import Data.Foldable
import qualified Data.IntMap as M
import Data.Maybe hiding (fromMaybe)
import Data.Monoid
import Data.Sequence
-- from hoodle-platform 
import Data.Hoodle.BBox
import Data.Hoodle.Generic
import Data.Hoodle.Predefined
import Data.Hoodle.Select
import Data.Hoodle.Simple (Dimension(..),Stroke(..))
import Graphics.Hoodle.Render.Generic
import Graphics.Hoodle.Render.Highlight
import Graphics.Hoodle.Render.Type
import Graphics.Hoodle.Render.Type.HitTest 
import Graphics.Hoodle.Render.Util 
-- from this package
import Hoodle.Type.Canvas
import Hoodle.Type.Alias 
import Hoodle.Util
import Hoodle.Type.PageArrangement
import Hoodle.Type.Predefined
import Hoodle.Type.Enum
import Hoodle.View.Coordinate
-- 
import Prelude hiding ((.),id,mapM_,concatMap,foldr)

-- | 
data DrawFlag = Clear | BkgEfficient | Efficient
              deriving (Eq,Ord,Show)
                       
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
              -- return () 
	  Just sfc -> do 
	    r <- renderWith sfc $ do 
	      clipBBox (fmap (flip inflate (-1.0)) mbbox')
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
        -- return ()      
  case ibbox of
    Top -> Just <$> action -- >> return Nothing 
    Middle _ -> Just <$> action -- >> return Nothing 
    Bottom -> return Nothing 

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
                    -> (DrawingArea, Maybe Surface)
                    -> CanvasGeometry 
                    -> Double 
                    -> (Double,Double,Double,Double) 
                    -> PageNum 
                    -> ((Double,Double),Double) 
                    -> ((Double,Double),Double) 
                    -> IO () 
drawCurvebitGen pmode (canvas,_msfc) geometry wdth (r,g,b,a) pnum ((x0,y0),z0) ((x,y),z) = do 
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win $ do
    cairoXform4PageCoordinate geometry pnum 
    setSourceRGBA r g b a
    case pmode of 
      NoPressure -> do 
        setLineWidth wdth
        moveTo x0 y0
        lineTo x y
        stroke
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
                -- clipBBox (fmap (flip inflate 1) mbboxnew) -- ad hoc ? 
                pg <- render (pnum,page) mbboxnew flag
                -- Start Widget
                when isCurrentCvs (emphasisCanvasRender ColorBlue geometry)  
                -- widget test
                -- let mbbox_canvas = fmap (xformBBox (unCvsCoord . desktop2Canvas geometry . DeskCoord )) mbboxnew 
                -- renderTestWidget mbbox_canvas (view (canvasWidgets.testWidgetPosition) cinfo)
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
                -- clipBBox (fmap (flip inflate 1) pgmbbox) -- ad hoc     
                render (pn,pg) pgmbbox flag
              renderfunc = do
                xformfunc 
                -- mapM_ onepagerender drawpgs 
                ndrawpgs <- mapM onepagerender drawpgs 
                let npgs = foldr rfunc pgs ndrawpgs   
                       where rfunc (k,pg) m = M.adjust (const pg) k m 
                let nhdl = set gpages npgs hdl  
                maybe (return ()) (\cpg->emphasisPageRender geometry (pnum,cpg)) mcpg 
                -- Start Widget 
                when isCurrentCvs (emphasisCanvasRender ColorRed geometry)
                -- widget test
                let mbbox_canvas = fmap (xformBBox (unCvsCoord . desktop2Canvas geometry . DeskCoord )) mbboxnew                 
                renderTestWidget mbbox_canvas (view (canvasWidgets.testWidgetPosition) cinfo) -- (CvsCoord (100,100))
                -- End Widget
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
                -- Start Widget
                when isCurrentCvs (emphasisCanvasRender ColorGreen geometry)  
                -- widget test
                let mbbox_canvas = fmap (xformBBox (unCvsCoord . desktop2Canvas geometry . DeskCoord )) mbboxnew                 
                renderTestWidget mbbox_canvas (view (canvasWidgets.testWidgetPosition) cinfo)
                -- End Widget 
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
drawSinglePageSel :: DrawingFunction SinglePage SelectMode         
drawSinglePageSel = drawFuncSelGen rendercontent renderselect
  where rendercontent (_pnum,tpg) mbbox flag = do
          let pg' = hPage2RPage tpg 
          case flag of 
            Clear -> cairoRenderOption (RBkgDrawPDF,DrawFull) pg' >> return ()
            BkgEfficient -> cairoRenderOption (InBBoxOption mbbox) (InBBoxBkgBuf pg') >> return ()            
            Efficient -> cairoRenderOption (InBBoxOption mbbox) (InBBox pg') >> return ()
          return ()
        renderselect (_pnum,tpg) mbbox _flag = do 
          cairoHittedBoxDraw tpg mbbox
          return ()

-- | 
drawContHoodle :: DrawingFunction ContinuousPage EditMode
drawContHoodle = drawContPageGen f  
  where f (PageNum n,page) _ Clear = (,) n <$> cairoRenderOption (RBkgDrawPDF,DrawFull) page 
        f (PageNum n,page) mbbox BkgEfficient = (,) n . unInBBoxBkgBuf <$> cairoRenderOption (InBBoxOption mbbox) (InBBoxBkgBuf page)                  
        f (PageNum n,page) mbbox Efficient = (,) n . unInBBox <$> cairoRenderOption (InBBoxOption mbbox) (InBBox page)


-- |
drawContHoodleSel :: DrawingFunction ContinuousPage SelectMode
drawContHoodleSel = drawContPageSelGen renderother renderselect 
  where renderother (PageNum n,page) mbbox flag = do
          case flag of 
            Clear -> (,) n <$> cairoRenderOption (RBkgDrawPDF,DrawFull) page 
            BkgEfficient -> (,) n . unInBBoxBkgBuf <$> cairoRenderOption (InBBoxOption mbbox) (InBBoxBkgBuf page)            
            Efficient -> (,) n . unInBBox <$> cairoRenderOption (InBBoxOption mbbox) (InBBox page)
        renderselect (PageNum n,tpg) mbbox _flag = do
          cairoHittedBoxDraw tpg mbbox 
          return (n,tpg)

-- |
cairoHittedBoxDraw :: Page SelectMode -> Maybe BBox -> Render () 
cairoHittedBoxDraw tpg mbbox = do   
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
        Middle bbox -> renderSelectHandle bbox 
        _ -> return () 
      resetClip
    Left _ -> return ()  

-- | 
renderLasso :: Seq (Double,Double) -> Render ()
renderLasso lst = do 
  setLineWidth predefinedLassoWidth
  uncurry4 setSourceRGBA predefinedLassoColor
  uncurry setDash predefinedLassoDash 
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
renderSelectHandle :: BBox -> Render () 
renderSelectHandle bbox = do 
  setLineWidth predefinedLassoWidth
  uncurry4 setSourceRGBA predefinedLassoColor
  uncurry setDash predefinedLassoDash 
  let (x1,y1) = bbox_upperleft bbox
      (x2,y2) = bbox_lowerright bbox
  rectangle x1 y1 (x2-x1) (y2-y1)
  stroke
  setSourceRGBA 1 0 0 0.8
  rectangle (x1-5) (y1-5) 10 10  
  fill
  setSourceRGBA 1 0 0 0.8
  rectangle (x1-5) (y2-5) 10 10  
  fill
  setSourceRGBA 1 0 0 0.8
  rectangle (x2-5) (y1-5) 10 10  
  fill
  setSourceRGBA 1 0 0 0.8
  rectangle (x2-5) (y2-5) 10 10  
  fill
  setSourceRGBA 0.5 0 0.2 0.8
  rectangle (x1-3) (0.5*(y1+y2)-3) 6 6  
  fill
  setSourceRGBA 0.5 0 0.2 0.8
  rectangle (x2-3) (0.5*(y1+y2)-3) 6 6  
  fill
  setSourceRGBA 0.5 0 0.2 0.8
  rectangle (0.5*(x1+x2)-3) (y1-3) 6 6  
  fill
  setSourceRGBA 0.5 0 0.2 0.8
  rectangle (0.5*(x1+x2)-3) (y2-3) 6 6  
  fill

renderTestWidget :: Maybe BBox -> CanvasCoordinate -> Render () 
renderTestWidget mbbox (CvsCoord (x,y)) = do 
  identityMatrix 
  clipBBox mbbox 
  setSourceRGBA 0.5 0.5 0.5 0.3 
  rectangle x y 100 100 
  fill 
  setSourceRGBA 0.7 0.2 0.2 0.5
  rectangle (x+30) (y+30) 40 40 
  fill 
  resetClip 


-- | 
canvasImageSurface :: CanvasGeometry 
                      -- -> Maybe Double  -- ^ multiply 
                      -> Hoodle EditMode 
                      -> IO (Surface,Surface)
canvasImageSurface geometry hdl = do 
  let ViewPortBBox bbx = getCanvasViewPort geometry 
      {- nbbx = case mmulti of
               Nothing -> bbx 
               Just z -> let (x0,y0) = bbox_upperleft bbx 
                             (x1,y1) = bbox_lowerright bbx
                             w = x1-x0
                             h = y1-y0 
                         in BBox (x0-z*w,y0-z*h) (x1+z*w,y0+z*h) -}
      BBox (x0,y0) (x1,y1) = xformBBox ( unCvsCoord . desktop2Canvas geometry . DeskCoord ) bbx 
      w = (x1-x0)
      h = (y1-y0)
  let pgs = view gpages hdl 
      drawpgs = (catMaybes . map f . getPagesInViewPortRange geometry) hdl 
        where f k = maybe Nothing (\a -> Just (k,a)) . M.lookup (unPageNum k) $ pgs
      onepagerender (pn,pg) = do 
        identityMatrix 
        cairoXform4PageCoordinate geometry pn
        cairoRenderOption (RBkgDrawPDF,DrawFull) pg
      renderfunc = do 
        setSourceRGBA 0.5 0.5 0.5 1
        rectangle 0 0 w h 
        fill 
        mapM_ onepagerender drawpgs 
  sfc <- createImageSurface FormatARGB32 (floor w) (floor h)
  sfc2 <- createImageSurface FormatARGB32 (floor w) (floor h)
  renderWith sfc renderfunc 
  return (sfc,sfc2) 
