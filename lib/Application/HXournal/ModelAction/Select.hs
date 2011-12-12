module Application.HXournal.ModelAction.Select where

import Application.HXournal.Type.Enum

import Graphics.Xournal.Type 
import Graphics.Xournal.Type.Select
import Graphics.Xournal.HitTest

import Graphics.UI.Gtk hiding (get,set)

import Data.Strict.Tuple
import qualified Data.IntMap as M
import qualified Data.Map as Map 


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
  let pgs = tx_pages txoj 
      pgs' = M.adjust (const (pageBBoxMapFromTempPageSelect tpage))
                        pagenum pgs
  in TempXournalSelect pgs' (Just (pagenum,tpage)) 
    
    
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
      
