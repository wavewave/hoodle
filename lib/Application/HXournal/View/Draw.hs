{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.View.Draw 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.View.Draw where

import Graphics.UI.Gtk hiding (get)
import Graphics.Rendering.Cairo

import Control.Applicative 
import Control.Category (id,(.))
import Control.Monad (liftM,(<=<))
import Data.Label
import Prelude hiding ((.),id,mapM_,concatMap)
import Data.Foldable
import Data.Monoid
import Data.Sequence
import Data.Xournal.Simple (Dimension(..))
import Data.Xournal.Generic
import Data.Xournal.BBox
import Graphics.Xournal.Render.Type
import Graphics.Xournal.Render.BBox 
import Graphics.Xournal.Render.BBoxMapPDF 
import Graphics.Xournal.Render.PDFBackground
import Graphics.Xournal.Render.Generic
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Alias 
import Application.HXournal.Device
import Application.HXournal.Util
import Application.HXournal.Type.PageArrangement
import Application.HXournal.Type.Predefined
import Application.HXournal.View.Coordinate


type DrawingFunction = ViewInfo SinglePage -> Maybe BBox -> IO ()

type PageDrawingFunction a = DrawingArea -> (PageNum,Page a) 
                             -> ViewInfo SinglePage -> Maybe BBox -> IO ()

{-                           
type PageDrawingFunctionForSelection 
      = DrawingArea -> (PageNum,Page SelectMode) -> ViewInfo SinglePage -> Maybe BBox -> IO ()
-}

-- | 

getCanvasViewPort :: CanvasGeometry -> ViewPortBBox 
getCanvasViewPort geometry = 
  let DeskCoord (x0,y0) = canvas2Desktop geometry (CvsCoord (0,0)) 
      CanvasDimension (Dim w h) = canvasDim geometry  
      DeskCoord (x1,y1) = canvas2Desktop geometry (CvsCoord (w,h))
  in ViewPortBBox (BBox (x0,y0) (x1,y1))

-- | 

getViewableBBox :: CanvasGeometry 
                   -> (PageNum, Maybe BBox) -- ^ in page coordinate
                   -> Maybe BBox            -- ^ in desktop coordinate
getViewableBBox geometry (pnum,mbbox) = 
  let ViewPortBBox vportbbox = getCanvasViewPort geometry  
  in toMaybe $ (fromMaybe mbbox :: IntersectBBox) 
               `mappend` 
               (Intersect (Middle vportbbox))


-- | common routine for double buffering 

doubleBufferDraw :: DrawWindow -> CanvasGeometry -> Render () -> Render () -> IO ()
doubleBufferDraw win geometry xform rndr = do 
  let Dim cw ch = unCanvasDimension . canvasDim $ geometry 
  withImageSurface FormatARGB32 (floor cw) (floor ch) $ \tempsurface -> do 
    renderWith tempsurface $ do 
      setSourceRGBA 0.5 0.5 0.5 1
      rectangle 0 0 cw ch 
      fill 
      rndr 
    renderWithDrawable win $ do 
      setSourceSurface tempsurface 0 0   
      setOperator OperatorSource 
      xform
      paint 


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

drawCurvebit :: DrawingArea 
               -> CanvasGeometry 
               -> Double 
               -> (Double,Double,Double,Double) 
               -> PageNum 
               -> (Double,Double) 
               -> (Double,Double) 
               -> IO () 
drawCurvebit canvas geometry wdth (r,g,b,a) pnum (x0,y0) (x,y) = do 
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win $ do
    cairoXform4PageCoordinate geometry pnum 
    setSourceRGBA r g b a
    setLineWidth wdth
    moveTo x0 y0
    lineTo x y
    stroke

-- | 
    
drawFuncGen :: (GPageable em) => em -> 
               ((PageNum,Page em) -> Maybe BBox -> Render ()) -> PageDrawingFunction em
drawFuncGen typ render canvas (pnum,page) vinfo mbbox = do 
    let arr = get pageArrangement vinfo
    geometry <- makeCanvasGeometry typ (pnum,page) arr canvas
    win <- widgetGetDrawWindow canvas
    let mbboxnew = getViewableBBox geometry (pnum,mbbox)
        xformfunc = cairoXform4PageCoordinate geometry pnum
        renderfunc = do
          xformfunc 
          clipBBox mbboxnew
          render (pnum,page) mbboxnew 
          resetClip 
    doubleBufferDraw win geometry xformfunc renderfunc   

drawFuncSelGen :: ((PageNum,Page SelectMode) -> Maybe BBox -> Render ()) 
                  -> ((PageNum,Page SelectMode) -> Maybe BBox -> Render ())
                  -> PageDrawingFunction SelectMode  
drawFuncSelGen rencont rensel = drawFuncGen SelectMode (\x y -> rencont x y >> rensel x y) 


drawPageClearly :: PageDrawingFunction EditMode
drawPageClearly = drawFuncGen EditMode $ \(_,page) _mbbox -> 
                     cairoRenderOption (DrawBkgPDF,DrawFull) (gcast page :: TPageBBoxMapPDF )


drawPageSelClearly :: PageDrawingFunction SelectMode         
drawPageSelClearly = drawFuncSelGen rendercontent renderselect 
  where rendercontent (_pnum,tpg)  _mbbox = do
          let pg' = gcast tpg :: Page EditMode
          cairoRenderOption (DrawBkgPDF,DrawFull) (gcast pg' :: TPageBBoxMapPDF)
        renderselect (_pnum,tpg) mbbox =  
          cairoHittedBoxDraw tpg mbbox

-- |

drawBuf :: PageDrawingFunction EditMode
drawBuf = drawFuncGen EditMode $ \(_,page) mbbox -> cairoRenderOption (InBBoxOption mbbox) (InBBox page) 
  
-- |

drawSelBuf :: PageDrawingFunction SelectMode
drawSelBuf = drawFuncSelGen rencont rensel  
  where rencont (_pnum,tpg) mbbox = do 
          let page = (gcast tpg :: Page EditMode)
          cairoRenderOption (InBBoxOption mbbox) (InBBox (gcast page :: TPageBBoxMapPDF))
        rensel (_pnum,tpg) mbbox = do 
          cairoHittedBoxDraw tpg mbbox  
             
-- |

cairoHittedBoxDraw :: Page SelectMode -> Maybe BBox -> Render () 
cairoHittedBoxDraw tpg mbbox = do   
  let layers = get g_layers tpg 
      slayer = gselectedlayerbuf layers 
  case unTEitherAlterHitted . get g_bstrokes $ slayer of
    Right alist -> do 
      clipBBox mbbox
      setSourceRGBA 0.0 0.0 1.0 1.0
      let hitstrs = concatMap unHitted (getB alist)
      mapM_ renderSelectedStroke hitstrs  
      let ulbbox = unUnion . mconcat . fmap (Union .Middle . strokebbox_bbox) 
                   $ hitstrs 
      case ulbbox of 
        Middle bbox -> renderSelectHandle bbox 
        _ -> return () 
      resetClip
    Left _ -> return ()  

-- |

getRatioPageCanvas :: ZoomMode -> PageDimension -> CanvasDimension -> (Double,Double)
getRatioPageCanvas zmode (PageDimension (Dim w h)) (CanvasDimension (Dim w' h')) = 
  case zmode of 
    Original -> (1.0,1.0)
    FitWidth -> (w'/w,w'/w)
    FitHeight -> (h'/h,h'/h)
    Zoom s -> (s,s)

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



renderBoxSelection :: BBox -> Render () 
renderBoxSelection bbox = do
  setLineWidth predefinedLassoWidth
  uncurry4 setSourceRGBA predefinedLassoColor
  uncurry setDash predefinedLassoDash 
  let (x1,y1) = bbox_upperleft bbox
      (x2,y2) = bbox_lowerright bbox
  rectangle x1 y1 (x2-x1) (y2-y1)
  stroke

renderSelectedStroke :: StrokeBBox -> Render () 
renderSelectedStroke str = do 
  setLineWidth 1.5
  setSourceRGBA 0 0 1 1
  cairoOneStrokeSelected str

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




{-  
  canvas page vinfo mbbox = do 
    let arr = get pageArrangement vinfo 
    geometry <- getCanvasPageGeometry canvas page origin
    win <- widgetGetDrawWindow canvas
    let mbboxnew = adjustBBoxWithView geometry zmode mbbox
        xformfunc = transformForPageCoord geometry zmode
        renderfunc = do
          xformfunc 
          clipBBox mbboxnew
          rencont page mbboxnew 
          rensel page mbboxnew 
          resetClip 
    doubleBuffering win geometry xformfunc renderfunc   

-}

{-             
             do 
  let zmode  = get zoomMode vinfo
      BBox origin _ = unViewPortBBox $ get (viewPortBBox.pageArrangement) vinfo
      page = (gcast tpg :: Page EditMode)
  geometry <- getCanvasPageGeometry canvas page origin
  win <- widgetGetDrawWindow canvas
  let xformfunc = transformForPageCoord geometry zmode 
  let renderfunc = do
        transformForPageCoord geometry zmode
        cairoRenderOption (InBBoxOption mbbox) (InBBox (gcast page :: TPageBBoxMapPDF))
        cairoHittedBoxDraw tpg mbbox  
  doubleBuffering win geometry xformfunc renderfunc   
-}


{-  
canvas page vinfo mbbox = do 
  let zmode  = get zoomMode vinfo
      BBox origin _ = unViewPortBBox $ get (viewPortBBox.pageArrangement) vinfo
  geometry <- getCanvasPageGeometry canvas page origin
  let mbboxnew = adjustBBoxWithView geometry zmode mbbox
  win <- widgetGetDrawWindow canvas
  let xformfunc = transformForPageCoord geometry zmode
  let renderfunc = do   
        xformfunc 
        cairoRenderOption (InBBoxOption mbboxnew) (InBBox page) 
        return ()
  doubleBuffering win geometry xformfunc renderfunc   -}



-- | obsolete
{-
data CanvasPageGeometry = 
  CanvasPageGeometry { screen_size :: (Double,Double) 
                     , canvas_size :: (Double,Double)
                     , page_size :: (Double,Double)
                     , canvas_origin :: (Double,Double) 
                     , page_origin :: (Double,Double)
                     }
  deriving (Show)  


-- | obsolete                            

type PageDrawF = DrawingArea -> Page EditMode 
                 -> ViewInfo SinglePage -> Maybe BBox -> IO ()

-- | obsolete

type PageDrawFSel = DrawingArea -> Page SelectMode -> ViewInfo SinglePage -> Maybe BBox -> IO ()


-- | obsolete 

getCanvasPageGeometry :: DrawingArea 
                         -> GPage b s a
                         -> (Double,Double) 
                         -> IO CanvasPageGeometry
getCanvasPageGeometry canvas page (xorig,yorig) = do 
  win <- widgetGetDrawWindow canvas
  (w',h') <- widgetGetSize canvas
  screen <- widgetGetScreen canvas
  (ws,hs) <- (,) <$> screenGetWidth screen <*> screenGetHeight screen
  let (Dim w h) = gdimension page
  (x0,y0) <- drawWindowGetOrigin win
  return $ CanvasPageGeometry (fromIntegral ws, fromIntegral hs) 
                              (fromIntegral w', fromIntegral h') 
                              (w,h) 
                              (fromIntegral x0,fromIntegral y0)
                              (xorig, yorig)

-- | obsolete

visibleViewPort :: CanvasPageGeometry -> ZoomMode -> BBox  
visibleViewPort cpg@(CanvasPageGeometry (_ws,_hs) (w',h') (_w,_h) (_x0,_y0) (xorig,yorig)) zmode = 
  let (xend,yend) = canvasToPageCoord cpg zmode (w',h')
  in  BBox (xorig,yorig) (xend,yend)


-- | obsolete 

core2pageCoord :: CanvasPageGeometry -> ZoomMode 
                  -> (Double,Double) -> (Double,Double)
core2pageCoord cpg@(CanvasPageGeometry (_ws,_hs) (_w',_h') (_w,_h) (_x0,_y0) (xorig,yorig))
               zmode (px,py) = 
  let s =  1.0 / getRatioFromPageToCanvas cpg zmode 
      (xo,yo) = case zmode of
                  Original -> (xorig,yorig)
                  FitWidth -> (0,yorig)
                  FitHeight -> (xorig,0)
                  _ -> error "not implemented yet in core2pageCoord"
  in (px*s+xo, py*s+yo)
  
-- | obsolete      

wacom2pageCoord :: CanvasPageGeometry 
                   -> ZoomMode 
                   -> (Double,Double) 
                   -> (Double,Double)
wacom2pageCoord cpg@(CanvasPageGeometry (ws,hs) (_w',_h') (_w,_h) (x0,y0) (xorig,yorig)) 
                zmode 
                (px,py) 
  = let (x1,y1) = (ws*px-x0,hs*py-y0)
        s = 1.0 / getRatioFromPageToCanvas cpg zmode
        (xo,yo) = case zmode of
                    Original -> (xorig,yorig)
                    FitWidth -> (0,yorig)
                    FitHeight -> (xorig,0)
                    _ -> error "not implemented wacom2pageCoord"
    in  (x1*s+xo,y1*s+yo)

-- | obsolete 

device2pageCoord :: CanvasPageGeometry 
                 -> ZoomMode 
                 -> PointerCoord  
                 -> (Double,Double)
device2pageCoord cpg zmode pcoord@(PointerCoord _ _ _)  = 
 let (px,py) = (,) <$> pointerX <*> pointerY $ pcoord  
 in case pointerType pcoord of 
      Core -> core2pageCoord  cpg zmode (px,py)
      _    -> wacom2pageCoord cpg zmode (px,py)
device2pageCoord _ _ NoPointerCoord = (-100,-100)

-- | obsolete 

pageToCanvasCoord :: CanvasPageGeometry -> ZoomMode -> (Double,Double) -> (Double,Double)
pageToCanvasCoord cpg@(CanvasPageGeometry _ _ _ _ (xorig,yorig)) zmode (x,y) = 
  let s = getRatioFromPageToCanvas cpg zmode
      (xo,yo) = case zmode of 
                  Original -> (xorig,yorig)
                  FitWidth -> (0,yorig)
                  FitHeight -> (xorig,0)
                  _ -> error "not implemented yet in pageToScreenCoord"
  in ((x-xo)*s,(y-yo)*s)

-- | obsolete 

canvasToPageCoord :: CanvasPageGeometry -> ZoomMode -> (Double,Double) -> (Double,Double) 
canvasToPageCoord = core2pageCoord

-- | obsolete

transformForPageCoord :: CanvasPageGeometry -> ZoomMode -> Render ()
transformForPageCoord cpg zmode = do 
  let (xo,yo) = page_origin cpg
  let s = getRatioFromPageToCanvas cpg zmode  
  scale s s
  translate (-xo) (-yo)      
  


  
  
                     

drawBBoxOnly :: PageDrawF
drawBBoxOnly canvas page vinfo _mbbox = do 
  let zmode  = get zoomMode vinfo
      BBox origin _ = unViewPortBBox $ get (viewPortBBox.pageArrangement) $ vinfo
  geometry <- getCanvasPageGeometry canvas page origin
  win <- widgetGetDrawWindow canvas
  let xformfunc = transformForPageCoord geometry zmode
      renderfunc = do 
        cairoRenderOption (DrawWhite,DrawBoxOnly) (gcast page :: TPageBBoxMapPDF)
  doubleBuffering win geometry xformfunc renderfunc   


-- | obsolete

adjustBBoxWithView :: CanvasPageGeometry -> ZoomMode -> Maybe BBox 
                      -> Maybe BBox
adjustBBoxWithView geometry zmode mbbox =   
  let viewbbox = visibleViewPort geometry zmode
  in  toMaybe $ (fromMaybe mbbox :: IntersectBBox)  
                `mappend` 
                (Intersect (Middle viewbbox))




drawPageInBBox :: PageDrawF 
drawPageInBBox canvas page vinfo mbbox = do 
  let zmode  = get zoomMode vinfo
      BBox origin _ = unViewPortBBox .get (viewPortBBox.pageArrangement) $ vinfo
  geometry <- getCanvasPageGeometry canvas page origin
  win <- widgetGetDrawWindow canvas
  let mbboxnew = adjustBBoxWithView geometry zmode mbbox
      xformfunc = transformForPageCoord geometry zmode
      renderfunc = do 
        cairoRenderOption (InBBoxOption mbboxnew) (InBBox page) 
        return ()
  doubleBuffering win geometry xformfunc renderfunc   


-- | deprecated

drawBBox :: PageDrawF 
drawBBox _ _ _ Nothing = return ()
drawBBox canvas page vinfo (Just bbox) = do 
  let zmode  = get zoomMode vinfo
      BBox origin _ = unViewPortBBox $ get (viewPortBBox.pageArrangement) vinfo
  geometry <- getCanvasPageGeometry canvas page origin
  win <- widgetGetDrawWindow canvas
  let xformfunc = transformForPageCoord geometry zmode
      renderfunc = do
        setLineWidth 0.5 
        setSourceRGBA 1.0 0.0 0.0 1.0
        xformfunc 
        let (x1,y1) = bbox_upperleft bbox
            (x2,y2) = bbox_lowerright bbox
        rectangle x1 y1 (x2-x1) (y2-y1)
        stroke
        return ()
  doubleBuffering win geometry xformfunc renderfunc   



-- | deprecated 

drawBBoxSel :: PageDrawFSel 
drawBBoxSel _ _ _ Nothing = return ()
drawBBoxSel canvas tpg vinfo (Just bbox) = do 
  let page = (gcast tpg :: Page EditMode)
  let zmode  = get zoomMode vinfo
      BBox origin _ = unViewPortBBox $ get (viewPortBBox.pageArrangement) vinfo
  geometry <- getCanvasPageGeometry canvas page origin
  win <- widgetGetDrawWindow canvas
  let xformfunc = transformForPageCoord geometry zmode
      renderfunc = do
        setLineWidth 0.5 
        setSourceRGBA 1.0 0.0 0.0 1.0
        let (x1,y1) = bbox_upperleft bbox
            (x2,y2) = bbox_lowerright bbox
        rectangle x1 y1 (x2-x1) (y2-y1)
        stroke
        return ()
  doubleBuffering win geometry xformfunc renderfunc   


-- | 

drawTempBBox :: BBox -> PageDrawF 
drawTempBBox _bbox _ _ _ Nothing = return ()
drawTempBBox bbox canvas page vinfo (Just _) = do 
  let zmode  = get zoomMode vinfo
      BBox origin _ = unViewPortBBox $ get (viewPortBBox.pageArrangement) vinfo
  geometry <- getCanvasPageGeometry canvas page origin
  win <- widgetGetDrawWindow canvas
  let xformfunc = transformForPageCoord geometry zmode
      renderbelow = do
        transformForPageCoord geometry zmode
        cairoRenderOption  (InBBoxOption Nothing) (InBBox page)
      renderabove = do
        setLineWidth 0.5 
        setSourceRGBA 1.0 0.0 0.0 1.0
        let (x1,y1) = bbox_upperleft bbox
            (x2,y2) = bbox_lowerright bbox
        rectangle x1 y1 (x2-x1) (y2-y1)
        stroke
        return ()
  doubleBuffering win geometry xformfunc (xformfunc >> renderbelow >> renderabove)
    



-- |

drawSelTempBBox :: BBox -> PageDrawFSel 
drawSelTempBBox _bbox _ _ _ Nothing = return ()
drawSelTempBBox bbox canvas tpg vinfo mbbox@(Just _) = do 
  let page = (gcast tpg :: Page EditMode)
  let zmode  = get zoomMode vinfo
      BBox origin _ = unViewPortBBox $ get (viewPortBBox.pageArrangement) vinfo
  geometry <- getCanvasPageGeometry canvas page origin
  win <- widgetGetDrawWindow canvas
  let xformfunc = transformForPageCoord geometry zmode
      renderbelow = do
        cairoRenderOption (InBBoxOption Nothing) (InBBox page)
        cairoHittedBoxDraw tpg mbbox  
      renderabove = do
        setLineWidth 0.5 
        setSourceRGBA 1.0 0.0 0.0 1.0
        let (x1,y1) = bbox_upperleft bbox
            (x2,y2) = bbox_lowerright bbox
        rectangle x1 y1 (x2-x1) (y2-y1)
        stroke
        return ()
  doubleBuffering win geometry xformfunc (xformfunc >> renderbelow >> renderabove)




-- | obsolete 

getRatioFromPageToCanvas :: CanvasPageGeometry -> ZoomMode -> Double 
getRatioFromPageToCanvas _cpg Original = 1.0 
getRatioFromPageToCanvas cpg FitWidth = 
  let (w,_)  = page_size cpg 
      (w',_) = canvas_size cpg 
  in  w'/w
getRatioFromPageToCanvas cpg FitHeight = 
  let (_,h)  = page_size cpg 
      (_,h') = canvas_size cpg 
  in  h'/h
getRatioFromPageToCanvas _cpg (Zoom s) = s 

-- | 


-- | obsolete 

drawSegment :: DrawingArea
               -> CanvasPageGeometry 
               -> ZoomMode 
               -> Double 
               -> (Double,Double,Double,Double) 
               -> (Double,Double) 
               -> (Double,Double) 
               -> IO () 
drawSegment canvas cpg zmode wdth (r,g,b,a) (x0,y0) (x,y) = do 
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win $ do
    transformForPageCoord cpg zmode
    setSourceRGBA r g b a
    setLineWidth wdth
    moveTo x0 y0
    lineTo x y
    stroke
  

showBBox :: DrawingArea -> CanvasPageGeometry -> ZoomMode -> BBox -> IO ()
showBBox canvas cpg zmode (BBox (ulx,uly) (lrx,lry)) = do 
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win $ do
    transformForPageCoord cpg zmode
    setSourceRGBA 0.0 1.0 0.0 1.0 
    setLineWidth  1.0 
    rectangle ulx uly (lrx-ulx) (lry-uly)    
    stroke
  return ()

dummyDraw :: PageDrawFSel 
dummyDraw _canvas _pgslct _vinfo _mbbox = do 
  putStrLn "dummy draw"
  return ()
  
  
drawSelectionInBBox :: PageDrawFSel 
drawSelectionInBBox canvas tpg vinfo mbbox = do 
  let zmode  = get zoomMode vinfo
      BBox origin _ = unViewPortBBox $ get (viewPortBBox.pageArrangement) vinfo
      page = (gcast tpg :: Page EditMode)
  geometry <- getCanvasPageGeometry canvas page origin
  win <- widgetGetDrawWindow canvas
  let mbboxnew = adjustBBoxWithView geometry zmode mbbox
  let xformfunc = transformForPageCoord geometry zmode
      renderfunc = do
        xformfunc 
        cairoRenderOption (InBBoxOption mbboxnew) (InBBox page)
        cairoHittedBoxDraw tpg mbboxnew  
  doubleBuffering win geometry xformfunc renderfunc   
    
  

---- 
    

-- | obsolete 

doubleBuffering :: DrawWindow -> CanvasPageGeometry 
                   -> Render ()
                   -> Render () 
                   -> IO ()
doubleBuffering win geometry xform rndr = do 
  let (cw, ch) = (,) <$> floor . fst <*> floor . snd 
                 $ canvas_size geometry 
  withImageSurface FormatARGB32 cw ch $ \tempsurface -> do 
    renderWith tempsurface $ do 
      setSourceRGBA 0.5 0.5 0.5 1
      rectangle 0 0 (fromIntegral cw) (fromIntegral ch) 
      fill 
      rndr 
    renderWithDrawable win $ do 
      setSourceSurface tempsurface 0 0   
      setOperator OperatorSource 
      xform
      paint 
  
  
-- |   

 

-}
