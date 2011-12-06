module Application.HXournal.ModelAction.Page where

import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Canvas

import Text.Xournal.Type
import Graphics.Xournal.Type
import Graphics.Xournal.Type.Map
import Graphics.Xournal.Type.Select

import Control.Category
import Data.Label
import Prelude hiding ((.),id)

import Data.Maybe
import qualified Data.IntMap as M 

updatePageAll :: XournalState 
                 -> HXournalState 
                 -> HXournalState
updatePageAll xst xstate = let cmap = get canvasInfoMap xstate
                               cmap' = fmap (updatePage xst) cmap
                           in  set canvasInfoMap cmap' xstate 


getPageFromXojBBoxMap :: Int -> XournalBBoxMap -> PageBBoxMap
getPageFromXojBBoxMap pagenum xojbbox  = 
  case M.lookup pagenum (xbm_pages xojbbox) of 
    Nothing -> error "something wrong in updatePage"
    Just p -> p

updatePage :: XournalState -> CanvasInfo -> CanvasInfo 
updatePage (ViewAppendState xojbbox) cinfo = 
  let pagenum = get currentPageNum cinfo 
      pg = getPageFromXojBBoxMap pagenum xojbbox 
      Dim w h = pageDim pg
  in  set currentPageNum pagenum 
      . set (pageDimension.viewInfo) (w,h)       
      . set currentPage (Left pg)
      $ cinfo 
updatePage (SelectState txoj) cinfo = 
  let pagenum = get currentPageNum cinfo
      mspage = tx_selectpage txoj 
      pageFromArg = case M.lookup pagenum (tx_pages txoj) of 
                      Nothing -> error "no such page in updatePage"
                      Just p -> p
      (newpage,Dim w h) = 
        case mspage of 
          Nothing -> (Left pageFromArg, pageDim pageFromArg)
          Just (spagenum,page) -> 
            if spagenum == pagenum 
              then (Right page, tp_dim page) 
              else (Left pageFromArg, pageDim pageFromArg)
  in set currentPageNum pagenum 
     . set (pageDimension.viewInfo) (w,h)       
     . set currentPage newpage
     $ cinfo 
  
setPage :: XournalState -> Int -> CanvasInfo -> CanvasInfo
setPage (ViewAppendState xojbbox) pagenum cinfo = 
  let pg = getPageFromXojBBoxMap pagenum xojbbox 
      Dim w h = pageDim pg
  in  set currentPageNum pagenum 
      . set (viewPortOrigin.viewInfo) (0,0) 
      . set (pageDimension.viewInfo) (w,h)       
      . set currentPage (Left pg)
      $ cinfo 
setPage (SelectState txoj) pagenum cinfo = 
  let mspage = tx_selectpage txoj 
      pageFromArg = case M.lookup pagenum (tx_pages txoj) of 
                      Nothing -> error "no such page in setPage"
                      Just p -> p
      (newpage,Dim w h) = 
        case mspage of 
          Nothing -> (Left pageFromArg, pageDim pageFromArg)
          Just (spagenum,page) -> 
            if spagenum == pagenum 
              then (Right page, tp_dim page) 
              else (Left pageFromArg, pageDim pageFromArg)
  in set currentPageNum pagenum 
     . set (viewPortOrigin.viewInfo) (0,0) 
     . set (pageDimension.viewInfo) (w,h)       
     . set currentPage newpage
     $ cinfo 
                            
getPage :: CanvasInfo -> PageBBoxMap
getPage cinfo = case get currentPage cinfo of 
                  Right tpgs -> pageBBoxMapFromTempPageSelect tpgs
                  Left pg -> pg 
                  