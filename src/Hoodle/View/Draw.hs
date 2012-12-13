{-# LANGUAGE GADTs, Rank2Types, TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.View.Draw 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.View.Draw where

-- import Control.Applicative ((<$>))
import Graphics.UI.Gtk hiding (get)
import Graphics.Rendering.Cairo
import Control.Category ((.))
import           Control.Lens
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
import Data.Hoodle.Simple (Dimension(..))
import Graphics.Hoodle.Render.Generic
import Graphics.Hoodle.Render.Highlight
import Graphics.Hoodle.Render.Type
import Graphics.Hoodle.Render.Type.HitTest 
import Graphics.Hoodle.Render.Util 
-- import Graphics.Hoodle.Render.Generic
-- from this package
import Hoodle.Type.Canvas
import Hoodle.Type.Alias 
import Hoodle.Util
import Hoodle.Type.PageArrangement
import Hoodle.Type.Predefined
import Hoodle.Type.Enum
import Hoodle.View.Coordinate
-- 
import Prelude hiding ((.),id,mapM_,concatMap)

-- | 
data DrawFlag = Clear | Efficient

-- | 
type family DrawingFunction v :: * -> * 

-- |

newtype SinglePageDraw a = 
  SinglePageDraw { unSinglePageDraw :: Bool 
                                       -> DrawingArea 
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

doubleBufferDraw :: DrawWindow -> CanvasGeometry -> Render () -> Render a 
                    -> IntersectBBox
                    -> IO (Maybe a)
doubleBufferDraw win geometry _xform rndr (Intersect ibbox) = do 
  let Dim cw ch = unCanvasDimension . canvasDim $ geometry 
      mbbox' = case ibbox of 
        Top -> Just (BBox (0,0) (cw,ch))
        Middle bbox -> Just (xformBBox (unCvsCoord . desktop2Canvas geometry . DeskCoord) bbox)
        Bottom -> Nothing 
  let action = withImageSurface FormatARGB32 (floor cw) (floor ch) $ 
        \tempsurface -> do 
          renderWith tempsurface $ do 
            setSourceRGBA 0.5 0.5 0.5 1
            rectangle 0 0 cw ch 
            fill 
            rndr 
          renderWithDrawable win $ do 
            clipBBox mbbox'
            setSourceSurface tempsurface 0 0   
            setOperator OperatorSource 
            paint 
  case ibbox of
    Top -> action >> return Nothing 
    Middle _ -> action >> return Nothing 
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
                    ->  DrawingArea 
                    -> CanvasGeometry 
                    -> Double 
                    -> (Double,Double,Double,Double) 
                    -> PageNum 
                    -> ((Double,Double),Double) 
                    -> ((Double,Double),Double) 
                    -> IO () 
drawCurvebitGen pmode canvas geometry wdth (r,g,b,a) pnum ((x0,y0),z0) ((x,y),z) = do 
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
  where func isCurrentCvs canvas (pnum,page) vinfo mbbox flag = do 
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
                when isCurrentCvs (emphasisCanvasRender ColorBlue geometry)  
                resetClip 
                return pg 
          doubleBufferDraw win geometry xformfunc renderfunc ibboxnew 
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
  setLineWidth 10
  rectangle 0 0 cw ch 
  stroke

-- |
drawContPageGen :: ((PageNum,Page EditMode) -> Maybe BBox -> DrawFlag -> Render ()) 
                   -> DrawingFunction ContinuousPage EditMode
drawContPageGen render = ContPageDraw func 
  where func isCurrentCvs cinfo mbbox hdl flag = do 
          let arr = view (viewInfo.pageArrangement) cinfo
              pnum = PageNum . view currentPageNum $ cinfo 
              canvas = view drawArea cinfo 
          geometry <- makeCanvasGeometry pnum arr canvas
          let pgs = view gpages hdl 
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
                mapM_ onepagerender drawpgs 
                when isCurrentCvs (emphasisCanvasRender ColorRed geometry)
                resetClip 
          doubleBufferDraw win geometry xformfunc renderfunc ibboxnew
          return hdl 


-- |
cairoBBox :: BBox -> Render () 
cairoBBox bbox = do 
  let (x1,y1) = bbox_upperleft bbox
      (x2,y2) = bbox_lowerright bbox
  rectangle x1 y1 (x2-x1) (y2-y1)
  stroke

-- |
drawContPageSelGen :: ((PageNum,Page EditMode) -> Maybe BBox -> DrawFlag -> Render ()) 
                      -> ((PageNum, Page SelectMode) -> Maybe BBox -> DrawFlag -> Render ())
                      -> DrawingFunction ContinuousPage SelectMode
drawContPageSelGen rendergen rendersel = ContPageDraw func 
  where func isCurrentCvs cinfo mbbox thdl flag = do 
          let arr = view (viewInfo.pageArrangement) cinfo
              pnum = PageNum . view currentPageNum $ cinfo 
              mtpage = view gselSelected thdl 
              canvas = view drawArea cinfo 
          geometry <- makeCanvasGeometry pnum arr canvas
          let pgs = view gselAll thdl 
              hdl = GHoodle (view gselTitle thdl) pgs 
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
              selpagerender (pn,pg) = do 
                identityMatrix 
                cairoXform4PageCoordinate geometry pn
                rendersel (pn,pg) (fmap (getBBoxInPageCoord geometry pn) mbboxnew) flag 
              renderfunc = do
                xformfunc 
                mapM_ onepagerender drawpgs 
                maybe (return ()) (\(n,tpage)-> selpagerender (PageNum n,tpage)) mtpage
                when isCurrentCvs (emphasisCanvasRender ColorGreen geometry)  
                resetClip 
          doubleBufferDraw win geometry xformfunc renderfunc ibboxnew
          return thdl


-- |
drawSinglePage :: DrawingFunction SinglePage EditMode
drawSinglePage = drawFuncGen EditMode f 
  where f (_,page) _ Clear = do 
          pg' <- cairoRenderOption (RBkgDrawPDF,DrawFull) page 
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
            Efficient -> cairoRenderOption (InBBoxOption mbbox) (InBBox pg') >> return ()
          return ()
        renderselect (_pnum,tpg) mbbox _flag = do 
          cairoHittedBoxDraw tpg mbbox
          return ()

-- | 
drawContHoodle :: DrawingFunction ContinuousPage EditMode
drawContHoodle = drawContPageGen f  
  where f (_,page) _ Clear = cairoRenderOption (RBkgDrawPDF,DrawFull) page 
                             >> return ()
        f (_,page) mbbox Efficient = cairoRenderOption (InBBoxOption mbbox) (InBBox page) 
                                     >> return ()

-- |
drawContHoodleSel :: DrawingFunction ContinuousPage SelectMode
drawContHoodleSel = drawContPageSelGen renderother renderselect 
  where renderother (_,page) mbbox flag = do
          case flag of 
            Clear -> cairoRenderOption (RBkgDrawPDF,DrawFull) page >> return ()
            Efficient -> cairoRenderOption (InBBoxOption mbbox) (InBBox page) >> return () 
          return ()
        renderselect (_pnum,tpg) mbbox _flag = do
          cairoHittedBoxDraw tpg mbbox 
          return ()


{-
-- |
drawBuf :: DrawingFunction SinglePage EditMode
drawBuf = drawFuncGen EditMode $ \(_,page) mbbox _ -> cairoRenderOption (InBBoxOption mbbox) (InBBox page) 
  
-- |
drawSelBuf :: DrawingFunction SinglePage SelectMode
drawSelBuf = drawFuncSelGen rencont rensel  
  where rencont (_pnum,tpg) mbbox _ = do 
          let page = hPage2RPage tpg 
          cairoRenderOption (InBBoxOption mbbox) (InBBox page)
        rensel (_pnum,tpg) mbbox _ = do 
          cairoHittedBoxDraw tpg mbbox  
             
-- | 
drawContHoodleBuf :: DrawingFunction ContinuousPage EditMode
drawContHoodleBuf = 
  drawContPageGen $ \(_,page) mbbox _ -> 
                       cairoRenderOption (InBBoxOption mbbox) (InBBox page)   
-}

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
      mapM_ renderSelectedItem hititms -- renderSelectedStroke 
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
renderSelectedStroke :: StrokeBBox -> Render () 
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

