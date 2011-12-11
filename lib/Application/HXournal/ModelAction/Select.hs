module Application.HXournal.ModelAction.Select where

import Graphics.Xournal.Type 
import Graphics.Xournal.Type.Select
import Graphics.Xournal.HitTest

import Data.Strict.Tuple
import qualified Data.IntMap as M

changeStrokeByOffset :: (Double,Double) -> StrokeBBox -> StrokeBBox 
changeStrokeByOffset (offx,offy) (StrokeBBox t c w ds bbox) = 
  let offset ( x :!: y )  = (x+offx) :!: (y+offy)
      newds = map offset ds 
      BBox (x1,y1) (x2,y2) = bbox 
      newbbox = BBox (x1+offx,y1+offy) (x2+offx,y2+offy)
  in  StrokeBBox t c w newds newbbox

changeSelectionByOffset :: TempPageSelect -> (Double,Double) -> TempPageSelect
changeSelectionByOffset tpage off = 
  let activelayer = tp_firstlayer tpage 
  in case strokes activelayer of 
       Left _ -> tpage 
       Right alist -> 
         let alist' =fmapAL id 
                            (Hitted . map (changeStrokeByOffset off) . unHitted) 
                            alist 
             layer' = LayerSelect (Right alist')
         in tpage { tp_firstlayer = layer' }

updateTempXournalSelect :: TempXournalSelect -> TempPageSelect -> Int 
                           -> TempXournalSelect
updateTempXournalSelect txoj tpage pagenum =                
  let pages = tx_pages txoj 
      pages' = M.adjust (const (pageBBoxMapFromTempPageSelect tpage))
                        pagenum pages
  in TempXournalSelect pages' (Just (pagenum,tpage)) 
    
    
hitInSelection :: TempPageSelect -> (Double,Double) -> Bool 
hitInSelection tpage point = 
  let activelayer = tp_firstlayer tpage
  in case strokes activelayer of 
       Left _ -> False   
       Right alist -> 
         let bboxes = map strokebbox_bbox . takeHittedStrokes $ alist
         in  any (flip hitTestBBoxPoint point) bboxes 
    
takeHittedStrokes :: AlterList [StrokeBBox] Hitted -> [StrokeBBox] 
takeHittedStrokes = concatMap unHitted . getB 

isAnyHitted :: AlterList [StrokeBBox] Hitted -> Bool 
isAnyHitted = null . takeHittedStrokes