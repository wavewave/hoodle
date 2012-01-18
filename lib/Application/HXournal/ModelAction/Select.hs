
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

changeStrokeByOffset :: (Double,Double) -> StrokeBBox -> StrokeBBox 
changeStrokeByOffset (offx,offy) (StrokeBBox t c w ds bbox) = 
  let offset ( x :!: y )  = (x+offx) :!: (y+offy)
      newds = map offset ds 
      BBox (x1,y1) (x2,y2) = bbox 
      newbbox = BBox (x1+offx,y1+offy) (x2+offx,y2+offy)
  in  StrokeBBox t c w newds newbbox

changeSelectionByOffset :: TTempPageSelectPDFBuf -> (Double,Double) 
                           -> TTempPageSelectPDFBuf
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
      
