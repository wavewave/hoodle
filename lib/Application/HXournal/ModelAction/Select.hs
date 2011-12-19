module Application.HXournal.ModelAction.Select where

import Application.HXournal.Type.Enum

import Data.Xournal.Simple
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


changeStrokeByOffset :: (Double,Double) -> StrokeBBox -> StrokeBBox 
changeStrokeByOffset (offx,offy) (StrokeBBox t c w ds bbox) = 
  let offset ( x :!: y )  = (x+offx) :!: (y+offy)
      newds = map offset ds 
      BBox (x1,y1) (x2,y2) = bbox 
      newbbox = BBox (x1+offx,y1+offy) (x2+offx,y2+offy)
  in  StrokeBBox t c w newds newbbox

changeSelectionByOffset :: TTempPageSelectPDF -> (Double,Double) -> TTempPageSelectPDF
changeSelectionByOffset tpage off = 
  let ls = glayers tpage
      activelayer = unTEitherAlterHitted . gstrokes . gselectedlayer $ ls
  in case activelayer of 
       Left _ -> tpage 
       Right alist -> 
         let alist' =fmapAL id 
                            (Hitted . map (changeStrokeByOffset off) . unHitted) 
                            alist 
             layer' = GLayer . TEitherAlterHitted . Right $ alist'
         in tpage { glayers = ls { gselectedlayer = layer' }}

updateTempXournalSelect :: TTempXournalSelectPDF -> TTempPageSelectPDF -> Int 
                           -> TTempXournalSelectPDF
updateTempXournalSelect txoj tpage pagenum =                
  let pgs = gselectAll txoj 
      pgs' = M.adjust (const (tpageBBoxMapPDFFromTTempPageSelectPDF tpage))
                        pagenum pgs
  in set g_selectAll pgs' 
     . set g_selectSelected (Just (pagenum,tpage))
     $ txoj 
     

    
    
hitInSelection :: TTempPageSelectPDF -> (Double,Double) -> Bool 
hitInSelection tpage point = 
  let activelayer = unTEitherAlterHitted . gstrokes .  gselectedlayer . glayers $ tpage
  in case activelayer of 
       Left _ -> False   
       Right alist -> 
         let bboxes = map strokebbox_bbox . takeHittedStrokes $ alist
         in  any (flip hitTestBBoxPoint point) bboxes 
    
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
      
