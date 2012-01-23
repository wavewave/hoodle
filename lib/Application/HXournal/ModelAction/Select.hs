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
module Application.HXournal.ModelAction.Select where

import Application.HXournal.Type.Enum
import Application.HXournal.Type.Canvas
import Application.HXournal.Draw

import Data.Monoid
import Data.Xournal.Generic
import Data.Xournal.BBox
import Graphics.Xournal.Render.Type
import Graphics.Xournal.Render.BBoxMapPDF
import Graphics.Xournal.Render.HitTest

import Graphics.UI.Gtk hiding (get,set)

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
      -- offsetx = nx1 - ox1
      -- offsety = ny1 - oy1
      nx = (x-ox1)*scalex+nx1
      ny = (y-oy1)*scaley+ny1
  in (nx,ny)

isBBoxDeltaSmallerThan :: Double ->CanvasPageGeometry -> ZoomMode 
                          -> BBox -> BBox -> Bool 
isBBoxDeltaSmallerThan delta cpg zmode 
                       (BBox (x11,y11) (x12,y12)) (BBox (x21,y21) (x22,y22)) = 
  let (x11',y11') = pageToCanvasCoord cpg zmode (x11,y11)
      (x12',y12') = pageToCanvasCoord cpg zmode (x12,y12)
      (x21',y21') = pageToCanvasCoord cpg zmode (x21,y21)
      (x22',y22') = pageToCanvasCoord cpg zmode (x22,y22)
  in (x11'-x21' > (-delta) && x11'-x21' < delta) 
     && (y11'-y21' > (-delta) && y11'-y21' < delta)  
     && (x12'-x22' > (-delta) && x12'-x22' < delta)
     && (y11'-y21' > (-delta) && y12'-y22' < delta)

changeStrokeBy :: ((Double,Double)->(Double,Double)) -> StrokeBBox -> StrokeBBox
changeStrokeBy func (StrokeBBox t c w ds bbox) = 
  let change ( x :!: y )  = let (nx,ny) = func (x,y) 
                            in nx :!: ny
      newds = map change ds 
      newbbox = mkbbox newds 
  in  StrokeBBox t c w newds newbbox

{-
changeStrokeByOffset :: (Double,Double) -> StrokeBBox -> StrokeBBox 
changeStrokeByOffset (offx,offy) (StrokeBBox t c w ds bbox) = 
  let offset ( x :!: y )  = (x+offx) :!: (y+offy)
      newds = map offset ds 
      BBox (x1,y1) (x2,y2) = bbox 
      newbbox = BBox (x1+offx,y1+offy) (x2+offx,y2+offy)
  in  StrokeBBox t c w newds newbbox
-}

changeSelectionByOffset :: (Double,Double) -> TTempPageSelectPDFBuf -> TTempPageSelectPDFBuf
changeSelectionByOffset (offx,offy) = changeSelectionBy (\(x,y)->(x+offx,y+offy))
{- 
changeSelectionByOffset tpage off = 
  let ls = glayers tpage
      slayer = gselectedlayerbuf ls
      buf = get g_buffer slayer 
      activelayer = unTEitherAlterHitted . get g_bstrokes $ slayer 
  in case activelayer of 
       Left _ -> tpage 
       Right alist -> 
         let alist' =fmapAL id 
                            (Hitted . map (changeStrokeByOffset off) . unHitted) 
                            alist 
             layer' = GLayerBuf buf . TEitherAlterHitted . Right $ alist'
         in tpage { glayers = ls { gselectedlayerbuf = layer' }}
-}


changeSelectionBy :: ((Double,Double) -> (Double,Double))
                     -> TTempPageSelectPDFBuf -> TTempPageSelectPDFBuf
changeSelectionBy func tpage = 
  let ls = glayers tpage
      slayer = gselectedlayerbuf ls
      buf = get g_buffer slayer 
      activelayer = unTEitherAlterHitted . get g_bstrokes $ slayer 
  in case activelayer of 
       Left _ -> tpage 
       Right alist -> 
         let alist' =fmapAL id 
                            (Hitted . map (changeStrokeBy func) . unHitted) 
                            alist 
             layer' = GLayerBuf buf . TEitherAlterHitted . Right $ alist'
         in tpage { glayers = ls { gselectedlayerbuf = layer' }}


updateTempXournalSelect :: TTempXournalSelectPDFBuf 
                           -> TTempPageSelectPDFBuf
                           -> Int 
                           -> TTempXournalSelectPDFBuf
updateTempXournalSelect txoj tpage pagenum =                
  let pgs = gselectAll txoj 
      pgs' = M.adjust (const (gcast tpage)) pagenum pgs
  in set g_selectAll pgs' 
     . set g_selectSelected (Just (pagenum,tpage))
     $ txoj 
     
updateTempXournalSelectIO :: TTempXournalSelectPDFBuf
                             -> TTempPageSelectPDFBuf
                             -> Int
                             -> IO TTempXournalSelectPDFBuf
updateTempXournalSelectIO txoj tpage pagenum = do   
  let pgs = gselectAll txoj 
  newpage <- resetPageBuffers (gcast tpage)
  let pgs' = M.adjust (const newpage) pagenum pgs
  return $  set g_selectAll pgs' 
            . set g_selectSelected (Just (pagenum,tpage))
            $ txoj 
  
    
hitInSelection :: TTempPageSelectPDFBuf -> (Double,Double) -> Bool 
hitInSelection tpage point = 
  let activelayer = unTEitherAlterHitted . get g_bstrokes .  gselectedlayerbuf . glayers $ tpage
  in case activelayer of 
       Left _ -> False   
       Right alist -> 
         let bboxes = map strokebbox_bbox . takeHittedStrokes $ alist
         in  any (flip hitTestBBoxPoint point) bboxes 
    
getULBBoxFromSelected :: TTempPageSelectPDFBuf -> ULMaybe BBox 
getULBBoxFromSelected tpage = 
  let activelayer = unTEitherAlterHitted . get g_bstrokes .  gselectedlayerbuf . glayers $ tpage
  in case activelayer of 
       Left _ -> Bottom
       Right alist -> 
         unUnion . mconcat . fmap (Union . Middle . strokebbox_bbox) . takeHittedStrokes $ alist 
     
hitInHandle :: TTempPageSelectPDFBuf -> (Double,Double) -> Bool 
hitInHandle tpage point = 
  case getULBBoxFromSelected tpage of 
    Middle bbox -> maybe False (const True) (checkIfHandleGrasped bbox point)
    _ -> False
    
{-  let activelayer = unTEitherAlterHitted . get g_bstrokes .  gselectedlayerbuf . glayers $ tpage
  in case activelayer of 
       Left _ -> False   
       Right alist -> 
         let ulbbox = unUnion . mconcat . fmap (Union . Middle . strokebbox_bbox) . takeHittledStroke $ alist 
         in case ulbbox of 
              Middle bbox -> maybe False (const True) (checkIfHandleGrasped bbox)
              _ -> False
         -- let bboxes = map strokebbox_bbox . takeHittedStrokes $ alist
         -- in  any (flip hitTestBBoxPoint point) bboxes 
-}



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
        f (B,x) (fs,ss) = (fs,ss)
        
getDiffStrokeBBox :: [StrokeBBox] -> [StrokeBBox] -> [(DI, StrokeBBox)]
getDiffStrokeBBox lst1 lst2 = 
  let nlst1 = fmap CmpStrokeBBox lst1 
      nlst2 = fmap CmpStrokeBBox lst2 
      diffresult = getDiff nlst1 nlst2 
  in map (\(x,y)->(x,unCmpStrokeBBox y)) diffresult

            
checkIfHandleGrasped :: BBox -> (Double,Double) -> Maybe Handle
checkIfHandleGrasped bbox@(BBox (ulx,uly) (lrx,lry)) (x,y)  
  | hitTestBBoxPoint (BBox (ulx-5,uly-5) (ulx+5,uly+5)) (x,y) = Just HandleTL
  | hitTestBBoxPoint (BBox (lrx-5,uly-5) (lrx+5,uly+5)) (x,y) = Just HandleTR
  | hitTestBBoxPoint (BBox (ulx-5,lry-5) (ulx+5,lry+5)) (x,y) = Just HandleBL
  | hitTestBBoxPoint (BBox (lrx-5,lry-5) (lrx+5,lry+5)) (x,y) = Just HandleBR
  | otherwise = Nothing  
                

                                                               