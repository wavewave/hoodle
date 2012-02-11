-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.ModelAction.Select 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.ModelAction.Select where

import Application.HXournal.Type.Enum
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Alias
import Application.HXournal.Type.Predefined 
import Application.HXournal.Type.PageArrangement
import Application.HXournal.View.Draw
import Application.HXournal.View.Coordinate
import Application.HXournal.Util
import Application.HXournal.ModelAction.Layer
import Data.Sequence (ViewL(..),viewl,Seq)
import Data.Foldable (foldl')
import Data.Monoid
import Data.Xournal.Generic
import Data.Xournal.BBox
import Data.Xournal.Simple (Dimension(..))
import Graphics.Xournal.Render.Type
import Graphics.Xournal.Render.BBox
import Graphics.Xournal.Render.BBoxMapPDF
import Graphics.Xournal.Render.HitTest
import Graphics.Xournal.Render.Simple
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Matrix (Matrix, invert, transformPoint )
import Graphics.UI.Gtk hiding (get,set)
import Data.Time.Clock
import Control.Monad

import Data.Strict.Tuple
import qualified Data.IntMap as M
import qualified Data.Map as Map 
import Control.Category
import Data.Label
import Prelude hiding ((.),id)
import Data.Algorithm.Diff


data Handle = HandleTL
            | HandleTR     
            | HandleBL
            | HandleBR
            | HandleTM
            | HandleBM
            | HandleML
            | HandleMR
            deriving (Show)
                     
scaleFromToBBox :: BBox -> BBox -> (Double,Double) -> (Double,Double)
scaleFromToBBox (BBox (ox1,oy1) (ox2,oy2)) (BBox (nx1,ny1) (nx2,ny2)) (x,y) = 
  let scalex = (nx2-nx1) / (ox2-ox1)
      scaley = (ny2-ny1) / (oy2-oy1) 
      nx = (x-ox1)*scalex+nx1
      ny = (y-oy1)*scaley+ny1
  in (nx,ny)

isBBoxDeltaSmallerThan :: Double -> PageNum -> CanvasGeometry -> BBox -> BBox -> Bool 
isBBoxDeltaSmallerThan delta pnum geometry 
                       (BBox (x11,y11) (x12,y12)) (BBox (x21,y21) (x22,y22)) =   
    let (x11',y11') = coordtrans (x11,y11)
        (x12',y12') = coordtrans (x12,y12)
        (x21',y21') = coordtrans (x21,y21)
        (x22',y22') = coordtrans (x22,y22)
    in (x11'-x21' > (-delta) && x11'-x21' < delta) 
       && (y11'-y21' > (-delta) && y11'-y21' < delta)  
       && (x12'-x22' > (-delta) && x12'-x22' < delta)
       && (y11'-y21' > (-delta) && y12'-y22' < delta)
  where coordtrans (x,y) = unCvsCoord . desktop2Canvas geometry . page2Desktop geometry 
                           $ (pnum,PageCoord (x,y))

-- | modify stroke using a function

changeStrokeBy :: ((Double,Double)->(Double,Double)) -> StrokeBBox -> StrokeBBox
changeStrokeBy func (StrokeBBox t c w ds _bbox) = 
  let change ( x :!: y )  = let (nx,ny) = func (x,y) 
                            in nx :!: ny
      newds = map change ds 
      newbbox = mkbbox newds 
  in  StrokeBBox t c w newds newbbox

getActiveLayer :: Page SelectMode -> Either [StrokeBBox] (TAlterHitted StrokeBBox)
getActiveLayer = unTEitherAlterHitted . get g_bstrokes . gselectedlayerbuf . get g_layers



getSelectedStrokes :: Page SelectMode -> [StrokeBBox]
getSelectedStrokes = either (const []) (concatMap unHitted . getB) . getActiveLayer   
  

-- | start a select mode with alter list selection 

makePageSelectMode :: Page EditMode  -- ^ base page 
                      -> TAlterHitted StrokeBBox -- ^ current selection layer (active layer will be replaced)
                      -> Page SelectMode -- ^ resultant select mode page
makePageSelectMode page alist =  
    let (mcurrlayer,npage) = getCurrentLayerOrSet page
        currlayer = maybeError "makePageSelectMode" mcurrlayer 
        newlayer = GLayerBuf (get g_buffer currlayer) (TEitherAlterHitted (Right alist))
        tpg = gcast npage 
        ls = get g_layers tpg 
        npg = tpg { glayers = ls { gselectedlayerbuf = newlayer}  }
    in npg 



-- | get unselected part of page and make an ordinary page

deleteSelected :: Page SelectMode -> Page SelectMode 
deleteSelected tpage =
    let activelayer = getActiveLayer tpage
        ls = glayers tpage
        buf = get g_buffer . gselectedlayerbuf $ ls 
    in case activelayer of 
         Left _ -> tpage 
         Right alist -> 
           let leftstrs = concat (getA alist)
               layer' = GLayerBuf buf . TEitherAlterHitted . Left $ leftstrs 
           in tpage { glayers = ls { gselectedlayerbuf = layer' }}   

-- | modify the whole selection using a function

changeSelectionBy :: ((Double,Double) -> (Double,Double))
                     -> Page SelectMode -> Page SelectMode
changeSelectionBy func tpage = 
  let activelayer = getActiveLayer tpage
      ls = glayers tpage
      buf = get g_buffer . gselectedlayerbuf $ ls 
  in case activelayer of 
       Left _ -> tpage 
       Right alist -> 
         let alist' =fmapAL id 
                            (Hitted . map (changeStrokeBy func) . unHitted) 
                            alist 
             layer' = GLayerBuf buf . TEitherAlterHitted . Right $ alist'
         in tpage { glayers = ls { gselectedlayerbuf = layer' }}
   

-- | special case of offset modification

changeSelectionByOffset :: (Double,Double) -> Page SelectMode -> Page SelectMode
changeSelectionByOffset (offx,offy) = changeSelectionBy (offsetFunc (offx,offy))

offsetFunc :: (Double,Double) -> (Double,Double) -> (Double,Double) 
offsetFunc (offx,offy) = \(x,y)->(x+offx,y+offy)



updateTempXournalSelect :: Xournal SelectMode -> Page SelectMode -> Int 
                           -> Xournal SelectMode 
updateTempXournalSelect txoj tpage pagenum =                
  let pgs = gselectAll txoj 
      pgs' = M.adjust (const (gcast tpage)) pagenum pgs
  in set g_selectAll pgs' 
     . set g_selectSelected (Just (pagenum,tpage))
     $ txoj 
     
updateTempXournalSelectIO :: Xournal SelectMode -> Page SelectMode -> Int
                             -> IO (Xournal SelectMode)
updateTempXournalSelectIO txoj tpage pagenum = do   
  let pgs = gselectAll txoj 
  newpage <- resetPageBuffers (gcast tpage)
  let pgs' = M.adjust (const newpage) pagenum pgs
  return $  set g_selectAll pgs' 
            . set g_selectSelected (Just (pagenum,tpage))
            $ txoj 
  
-- |   
  
calculateWholeBBox :: [StrokeBBox] -> Maybe BBox  
calculateWholeBBox = toMaybe . mconcat . map ( Union . Middle. strokebbox_bbox ) 
  
-- |     
hitInSelection :: Page SelectMode -> (Double,Double) -> Bool 
hitInSelection tpage point = 
  let activelayer = unTEitherAlterHitted . get g_bstrokes .  gselectedlayerbuf . glayers $ tpage
  in case activelayer of 
       Left _ -> False   
       Right alist -> 
         let Union bboxall = mconcat
                             . map ( Union . Middle. strokebbox_bbox ) 
                             . takeHittedStrokes $ alist
         
         in  case bboxall of 
               Middle bbox -> hitTestBBoxPoint bbox point 
               _ -> False 
          
    
getULBBoxFromSelected :: Page SelectMode -> ULMaybe BBox 
getULBBoxFromSelected tpage = 
  let activelayer = unTEitherAlterHitted . get g_bstrokes .  gselectedlayerbuf . glayers $ tpage
  in case activelayer of 
       Left _ -> Bottom
       Right alist -> 
         unUnion . mconcat . fmap (Union . Middle . strokebbox_bbox) . takeHittedStrokes $ alist 
     
hitInHandle :: Page SelectMode -> (Double,Double) -> Bool 
hitInHandle tpage point = 
  case getULBBoxFromSelected tpage of 
    Middle bbox -> maybe False (const True) (checkIfHandleGrasped bbox point)
    _ -> False
    
takeHittedStrokes :: AlterList [StrokeBBox] (Hitted StrokeBBox) -> [StrokeBBox] 
takeHittedStrokes = concatMap unHitted . getB 

isAnyHitted :: AlterList [StrokeBBox] (Hitted StrokeBBox) -> Bool 
isAnyHitted = not . null . takeHittedStrokes

toggleCutCopyDelete :: UIManager -> Bool -> IO ()
toggleCutCopyDelete ui b = do 
    agr <- uiManagerGetActionGroups ui >>= \x -> 
      case x of
        [] -> error "No action group?"
        y:_ -> return y
    Just deletea <- actionGroupGetAction agr "DELETEA"
    Just copya <- actionGroupGetAction agr "COPYA"
    Just cuta <- actionGroupGetAction agr "CUTA"
    let copycutdeletea = [copya,cuta,deletea] 
    mapM_ (flip actionSetSensitive b) copycutdeletea

togglePaste :: UIManager -> Bool -> IO ()
togglePaste ui b = do 
    agr <- uiManagerGetActionGroups ui >>= \x -> 
      case x of
        [] -> error "No action group?"
        y:_ -> return y
    Just pastea <- actionGroupGetAction agr "PASTEA"
    actionSetSensitive pastea b


changeStrokeColor :: PenColor -> StrokeBBox -> StrokeBBox
changeStrokeColor pcolor str =
  let Just cname = Map.lookup pcolor penColorNameMap 
  in  str { strokebbox_color = cname } 
      

changeStrokeWidth :: Double -> StrokeBBox -> StrokeBBox
changeStrokeWidth pwidth str = str { strokebbox_width = pwidth } 
      
newtype CmpStrokeBBox = CmpStrokeBBox { unCmpStrokeBBox :: StrokeBBox }
                      deriving Show
instance Eq CmpStrokeBBox where
  CmpStrokeBBox str1 == CmpStrokeBBox str2 = strokebbox_bbox str1 == strokebbox_bbox str2  
  
isSame :: DI -> Bool   
isSame B = True 
isSame _ = False 

separateFS :: [(DI,a)] -> ([a],[a])
separateFS = foldr f ([],[]) 
  where f (F,x) (fs,ss) = (x:fs,ss)
        f (S,x) (fs,ss) = (fs,x:ss)
        f (B,_x) (fs,ss) = (fs,ss)
        
getDiffStrokeBBox :: [StrokeBBox] -> [StrokeBBox] -> [(DI, StrokeBBox)]
getDiffStrokeBBox lst1 lst2 = 
  let nlst1 = fmap CmpStrokeBBox lst1 
      nlst2 = fmap CmpStrokeBBox lst2 
      diffresult = getDiff nlst1 nlst2 
  in map (\(x,y)->(x,unCmpStrokeBBox y)) diffresult

            
checkIfHandleGrasped :: BBox -> (Double,Double) -> Maybe Handle
checkIfHandleGrasped (BBox (ulx,uly) (lrx,lry)) (x,y)  
  | hitTestBBoxPoint (BBox (ulx-5,uly-5) (ulx+5,uly+5)) (x,y) = Just HandleTL
  | hitTestBBoxPoint (BBox (lrx-5,uly-5) (lrx+5,uly+5)) (x,y) = Just HandleTR
  | hitTestBBoxPoint (BBox (ulx-5,lry-5) (ulx+5,lry+5)) (x,y) = Just HandleBL
  | hitTestBBoxPoint (BBox (lrx-5,lry-5) (lrx+5,lry+5)) (x,y) = Just HandleBR
  | hitTestBBoxPoint (BBox (0.5*(ulx+lrx)-5,uly-5) (0.5*(ulx+lrx)+5,uly+5)) (x,y) = Just HandleTM  
  | hitTestBBoxPoint (BBox (0.5*(ulx+lrx)-5,lry-5) (0.5*(ulx+lrx)+5,lry+5)) (x,y) = Just HandleBM
  | hitTestBBoxPoint (BBox (ulx-5,0.5*(uly+lry)-5) (ulx+5,0.5*(uly+lry)+5)) (x,y) = Just HandleML
  | hitTestBBoxPoint (BBox (lrx-5,0.5*(uly+lry)-5) (lrx+5,0.5*(uly+lry)+5)) (x,y) = Just HandleMR
  | otherwise = Nothing  
                
getNewBBoxFromHandlePos :: Handle -> BBox -> (Double,Double) -> BBox
getNewBBoxFromHandlePos handle (BBox (ox1,oy1) (ox2,oy2)) (x,y) =           
    case handle of
      HandleTL -> BBox (x,y) (ox2,oy2)
      HandleTR -> BBox (ox1,y) (x,oy2)
      HandleBL -> BBox (x,oy1) (ox2,y)
      HandleBR -> BBox (ox1,oy1) (x,y)
      HandleTM -> BBox (ox1,y) (ox2,oy2)
      HandleBM -> BBox (ox1,oy1) (ox2,y)
      HandleML -> BBox (x,oy1) (ox2,oy2)
      HandleMR -> BBox (ox1,oy1) (x,oy2)


angleBAC :: (Double,Double) -> (Double,Double) -> (Double,Double) -> Double 
angleBAC (bx,by) (ax,ay) (cx,cy) = 
    let theta1 | ax==bx && ay>by = pi/2.0 
               | ax==bx && ay<=by = -pi/2.0 
               | ax<bx && ay>by = atan ((ay-by)/(ax-bx)) + pi 
               | ax<bx && ay<=by = atan ((ay-by)/(ax-bx)) - pi
               | otherwise = atan ((ay-by)/(ax-bx)) 
        theta2 | cx==bx && cy>by = pi/2.0 
               | cx==bx && cy<=by = -pi/2.0                                         
               | cx<bx && cy>by = atan ((cy-by)/(cx-bx)) +pi
               | cx<bx && cy<=by = atan ((cy-by)/(cx-bx)) - pi
               | otherwise = atan ((cy-by)/(cx-bx))
        dtheta = theta2 - theta1 
        result | dtheta > pi = dtheta - 2.0*pi
               | dtheta < (-pi) = dtheta + 2.0*pi
               | otherwise = dtheta
    in result 


wrappingAngle :: Seq (Double,Double) -> (Double,Double) -> Double
wrappingAngle lst p = 
    case viewl lst of 
      EmptyL -> 0 
      x :< xs -> Prelude.snd $ foldl' f (x,0) xs 
  where f (q',theta) q = let theta' = angleBAC p q' q
                         in theta' `seq` (q,theta'+theta)  
                          
mappingDegree :: Seq (Double,Double) -> (Double,Double) -> Int                        
mappingDegree lst = round . (/(2.0*pi)) . wrappingAngle lst 


hitLassoPoint :: Seq (Double,Double) -> (Double,Double) -> Bool 
hitLassoPoint lst = odd . mappingDegree lst

hitLassoStroke :: Seq (Double,Double) -> StrokeBBox -> Bool 
hitLassoStroke lst strk = all (\(x :!: y)-> hitLassoPoint lst (x,y)) $ strokebbox_data strk


data TempSelectRender a = TempSelectRender { tempSurface :: Surface  
                                           , widthHeight :: (Double,Double)
                                           , tempSelectInfo :: a 
                                           } 

type TempSelection = TempSelectRender [StrokeBBox]

data StrokesNImage = StrokesNImage { strokes :: [StrokeBBox]
                                   , strbbox :: Maybe BBox 
                                   , imageSurface :: Surface } 


-- | 

mkStrokesNImage :: CanvasGeometry -> Page SelectMode -> IO StrokesNImage 
mkStrokesNImage geometry tpage = do 
  let strs = getSelectedStrokes tpage
      drawselection = mapM_ (drawOneStroke.gToStroke) strs 
      Dim cw ch = get g_dimension tpage -- unCanvasDimension . canvasDim $ geometry 
      mbbox = case getULBBoxFromSelected tpage of 
                Middle bbox -> Just bbox 
                _ -> Nothing 
  sfc <- createImageSurface FormatARGB32 (floor cw) (floor ch) 
  renderWith sfc $ do 
    setSourceRGBA 1 1 1 0    
    rectangle 0 0 cw ch 
    fill 
    setSourceRGBA 0 0 0 1
    drawselection
  return $ StrokesNImage strs mbbox sfc

-- | 
          
drawTempSelectImage :: CanvasGeometry 
                       -> TempSelectRender StrokesNImage 
                       -> Matrix -- ^ transformation matrix
                       -> Render ()
drawTempSelectImage geometry tempselection xformmat = do 
    let sfc = imageSurface (tempSelectInfo tempselection)
        CanvasDimension (Dim cw ch) = canvasDim geometry 
        invxformmat = invert xformmat 
        newvbbox = BBox (transformPoint invxformmat (0,0)) 
                        (transformPoint invxformmat (cw,ch))
        mbbox = strbbox (tempSelectInfo tempselection)
        newmbbox = case unIntersect (Intersect (Middle newvbbox) `mappend` fromMaybe mbbox) of 
                     Middle bbox -> Just bbox 
                     _ -> Just newvbbox
    setMatrix xformmat
    clipBBox newmbbox
    setSourceSurface sfc 0 0 
    setOperator OperatorOver
    paint 



-- | 

tempSelected :: TempSelection -> [StrokeBBox]
tempSelected = tempSelectInfo 

mkTempSelection :: Surface -> (Double,Double) -> [StrokeBBox] -> TempSelection
mkTempSelection sfc (w,h) strs = TempSelectRender sfc (w,h) strs 

-- | update the content of temp selection. should not be often updated
   
updateTempSelection :: TempSelectRender a -> Render () -> Bool -> IO ()
updateTempSelection tempselection  renderfunc isFullErase = 
  renderWith (tempSurface tempselection) $ do 
    when isFullErase $ do 
      let (cw,ch) = widthHeight tempselection
      setSourceRGBA 0.5 0.5 0.5 1
      rectangle 0 0 cw ch 
      fill 
    renderfunc    
    
-- | 
    
getNewCoordTime :: ((Double,Double),UTCTime) 
                   -> (Double,Double)
                   -> IO (Bool,((Double,Double),UTCTime))
getNewCoordTime (prev,otime) (x,y) = do 
    ntime <- getCurrentTime 
    let dtime = diffUTCTime ntime otime 
        willUpdate = dtime > dtime_bound
        (nprev,nntime) = if dtime > dtime_bound 
                         then ((x,y),ntime)
                         else (prev,otime)
    return (willUpdate,(nprev,nntime))

