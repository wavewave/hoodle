module Application.HXournal.Draw where

import Graphics.UI.Gtk hiding (get)
import Graphics.Rendering.Cairo

import Control.Applicative 
import Control.Category
import Data.Label
import Prelude hiding ((.),id)

import Data.Monoid
import Data.Xournal.Simple
import Data.Xournal.Generic

import Data.Xournal.BBox
import Graphics.Xournal.Render.Type

import Graphics.Xournal.Render.BBox 
import Graphics.Xournal.Render.BBoxMapPDF 
import Graphics.Xournal.Render.PDFBackground
import Graphics.Xournal.Render.Generic
import Graphics.Xournal.Render.HitTest
import Application.HXournal.Type 
import Application.HXournal.Device



data CanvasPageGeometry = 
  CanvasPageGeometry { screen_size :: (Double,Double) 
                     , canvas_size :: (Double,Double)
                     , page_size :: (Double,Double)
                     , canvas_origin :: (Double,Double) 
                     , page_origin :: (Double,Double)
                     }
  deriving (Show)  

type PageDrawF = DrawingArea -> TPageBBoxMapPDFBuf -> ViewInfo -> Maybe BBox 
                 -> IO ()

type PageDrawFSel = DrawingArea -> TTempPageSelectPDFBuf -> ViewInfo -> Maybe BBox 
                    -> IO ()


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

visibleViewPort :: CanvasPageGeometry -> ZoomMode -> BBox  
visibleViewPort cpg@(CanvasPageGeometry (_ws,_hs) (w',h') (_w,_h) (_x0,_y0) (xorig,yorig)) zmode = 
  let (xend,yend) = canvasToPageCoord cpg zmode (w',h')
  in  BBox (xorig,yorig) (xend,yend)


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

pageToCanvasCoord :: CanvasPageGeometry -> ZoomMode -> (Double,Double) -> (Double,Double)
pageToCanvasCoord cpg@(CanvasPageGeometry _ _ _ _ (xorig,yorig)) zmode (x,y) = 
  let s = getRatioFromPageToCanvas cpg zmode
      (xo,yo) = case zmode of 
                  Original -> (xorig,yorig)
                  FitWidth -> (0,yorig)
                  FitHeight -> (xorig,0)
                  _ -> error "not implemented yet in pageToScreenCoord"
  in ((x-xo)*s,(y-yo)*s)

canvasToPageCoord :: CanvasPageGeometry -> ZoomMode -> (Double,Double) -> (Double,Double) 
canvasToPageCoord = core2pageCoord

transformForPageCoord :: CanvasPageGeometry -> ZoomMode -> Render ()
transformForPageCoord cpg zmode = do 
  let (xo,yo) = page_origin cpg
  let s = getRatioFromPageToCanvas cpg zmode  
  scale s s
  translate (-xo) (-yo)      
  

drawFuncGen :: (TPageBBoxMapPDFBuf -> Maybe BBox -> Render ()) -> PageDrawF 
drawFuncGen render canvas page vinfo mbbox = do 
  let zmode  = get zoomMode vinfo
      origin = get viewPortOrigin vinfo
  geometry <- getCanvasPageGeometry canvas page origin
  win <- widgetGetDrawWindow canvas
  let mbboxnew = adjustBBoxWithView geometry zmode mbbox
  renderWithDrawable win $ do
    transformForPageCoord geometry zmode
    clipBBox mbboxnew
    render page mbboxnew 
    resetClip 
    return ()
  return ()
  
drawFuncSelGen :: (TTempPageSelectPDFBuf -> Maybe BBox -> Render ()) 
                  -> (TTempPageSelectPDFBuf -> Maybe BBox -> Render ())
                  -> PageDrawFSel  
drawFuncSelGen rencont rensel canvas page vinfo mbbox = do 
  let zmode  = get zoomMode vinfo
      origin = get viewPortOrigin vinfo
  geometry <- getCanvasPageGeometry canvas page origin
  win <- widgetGetDrawWindow canvas
  let mbboxnew = adjustBBoxWithView geometry zmode mbbox
  renderWithDrawable win $ do
    transformForPageCoord geometry zmode
    clipBBox mbboxnew
    rencont page mbboxnew 
    rensel page mbboxnew 
    resetClip 
  return ()
  


drawPageClearly :: PageDrawF
drawPageClearly = drawFuncGen $ \page _mbbox -> 
                     cairoRenderOption (DrawBkgPDF,DrawFull) (gcast page :: TPageBBoxMapPDF)
                     
drawPageSelClearly :: PageDrawFSel                      
drawPageSelClearly = drawFuncSelGen rendercontent renderselect 
  where rendercontent tpg _mbbox = do
          let pg = (gcast tpg :: TPageBBoxMapPDFBuf)
          cairoRenderOption (DrawBkgPDF,DrawFull) (gcast pg :: TPageBBoxMapPDF)
        renderselect tpg mbbox = 
          cairoHittedBoxDraw tpg mbbox




drawBBoxOnly :: PageDrawF
drawBBoxOnly canvas page vinfo _mbbox = do 
  let zmode  = get zoomMode vinfo
      origin = get viewPortOrigin vinfo
  geometry <- getCanvasPageGeometry canvas page origin
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win $ do
    transformForPageCoord geometry zmode
    cairoRenderOption (DrawWhite,DrawBoxOnly) (gcast page :: TPageBBoxMapPDF)
     --  tpageBBoxMapPDFFromTPageBBoxMapPDFBuf page )
  return ()

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
      origin = get viewPortOrigin vinfo
  geometry <- getCanvasPageGeometry canvas page origin
  win <- widgetGetDrawWindow canvas
  let mbboxnew = adjustBBoxWithView geometry zmode mbbox
  renderWithDrawable win $ do
    transformForPageCoord geometry zmode
    cairoRenderOption (InBBoxOption mbboxnew) (InBBox page) --  (InBBox (gcast page :: TPageBBoxMapPDF))
    return ()
  return ()

drawBBox :: PageDrawF 
drawBBox _ _ _ Nothing = return ()
drawBBox canvas page vinfo (Just bbox) = do 
  let zmode  = get zoomMode vinfo
      origin = get viewPortOrigin vinfo
  geometry <- getCanvasPageGeometry canvas page origin
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win $ do
    setLineWidth 0.5 
    setSourceRGBA 1.0 0.0 0.0 1.0
    transformForPageCoord geometry zmode
    let (x1,y1) = bbox_upperleft bbox
        (x2,y2) = bbox_lowerright bbox
    rectangle x1 y1 (x2-x1) (y2-y1)
    stroke
  return ()

drawBBoxSel :: PageDrawFSel 
drawBBoxSel _ _ _ Nothing = return ()
drawBBoxSel canvas tpg vinfo (Just bbox) = do 
  let page = (gcast tpg :: TPageBBoxMapPDFBuf)
  let zmode  = get zoomMode vinfo
      origin = get viewPortOrigin vinfo
  geometry <- getCanvasPageGeometry canvas page origin
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win $ do
    setLineWidth 0.5 
    setSourceRGBA 1.0 0.0 0.0 1.0
    transformForPageCoord geometry zmode
    let (x1,y1) = bbox_upperleft bbox
        (x2,y2) = bbox_lowerright bbox
    rectangle x1 y1 (x2-x1) (y2-y1)
    stroke
  return ()


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
      origin = get viewPortOrigin vinfo
      page = (gcast tpg :: TPageBBoxMapPDFBuf)
  geometry <- getCanvasPageGeometry canvas page origin
  win <- widgetGetDrawWindow canvas
  let mbboxnew = adjustBBoxWithView geometry zmode mbbox
  renderWithDrawable win $ do
    transformForPageCoord geometry zmode
    cairoRenderOption (InBBoxOption mbboxnew) (InBBox page)
       -- (InBBox (gcast page :: TPageBBoxMapPDF))
    cairoHittedBoxDraw tpg mbboxnew  
      
{-
drawSelectionInBBoxOnly :: PageDrawFSel 
drawSelectionInBBoxOnly canvas tpg vinfo mbbox = do 
  let zmode  = get zoomMode vinfo
      origin = get viewPortOrigin vinfo
      page = (gcast tpg :: TPageBBoxMapPDFBuf)
  geometry <- getCanvasPageGeometry canvas page origin
  win <- widgetGetDrawWindow canvas  
  let mbboxnew = adjustBBoxWithView geometry zmode mbbox
  renderWithDrawable win $ do
    transformForPageCoord geometry zmode
    cairoRenderOption (InBBoxOption mbboxnew) (InBBox page)
      -- (DrawWhite,DrawBoxOnly) (gcast page :: TPageBBoxMapPDF)
      -- (DrawBuffer,DrawBoxOnly) (gcast page :: TPageBBoxMapPDF)
    cairoHittedBoxDraw tpg mbbox 
-}
  
cairoHittedBoxDraw :: TTempPageSelectPDFBuf -> Maybe BBox -> Render () 
cairoHittedBoxDraw tpg mbbox = do   
  let layers = get g_layers tpg 
      slayer = gselectedlayerbuf layers 
  case unTEitherAlterHitted . get g_bstrokes $ slayer of
    Right alist -> do 
      clipBBox mbbox
      setSourceRGBA 0.0 0.0 1.0 1.0
      let hitstrs = concatMap unHitted (getB alist)
          oneboxdraw str = do 
            let bbox@(BBox (x1,y1) (x2,y2)) = strokebbox_bbox str
                drawbox = do { rectangle x1 y1 (x2-x1) (y2-y1); stroke }
            case mbbox of 
              Just bboxarg -> if hitTestBBoxBBox bbox bboxarg 
                              then drawbox
                              else return () 
              Nothing -> drawbox 
      mapM_ oneboxdraw hitstrs                       
      resetClip
    Left _ -> return ()  

---- 
    
drawBuf :: PageDrawF 
drawBuf canvas page vinfo mbbox = do 
  let zmode  = get zoomMode vinfo
      origin = get viewPortOrigin vinfo
  geometry <- getCanvasPageGeometry canvas page origin
  let mbboxnew = adjustBBoxWithView geometry zmode mbbox
{-  let viewbbox = visibleViewPort geometry zmode
      newmbbox = toMaybe $ (fromMaybe mbbox :: IntersectBBox)  `mappend` (Intersect (Middle viewbbox)) -}
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win $ do
    transformForPageCoord geometry zmode
    cairoRenderOption (InBBoxOption mbboxnew) (InBBox page) 
    return ()
  return ()


drawSelBuf :: PageDrawFSel 
drawSelBuf canvas tpg vinfo mbbox = do 
  let zmode  = get zoomMode vinfo
      origin = get viewPortOrigin vinfo
      page = (gcast tpg :: TPageBBoxMapPDFBuf)
  geometry <- getCanvasPageGeometry canvas page origin
  win <- widgetGetDrawWindow canvas
  renderWithDrawable win $ do
    transformForPageCoord geometry zmode
    cairoRenderOption (InBBoxOption mbbox) (InBBox (gcast page :: TPageBBoxMapPDF))
    cairoHittedBoxDraw tpg mbbox  
