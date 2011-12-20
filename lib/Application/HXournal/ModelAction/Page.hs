module Application.HXournal.ModelAction.Page where

import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Canvas
import Data.Xournal.Simple
import Data.Xournal.Map
import Data.Xournal.Generic
import Graphics.Xournal.Render.Generic
import Graphics.Xournal.Render.BBoxMapPDF

import Control.Category
import Data.Label
import Prelude hiding ((.),id)
import qualified Data.IntMap as M 

updatePageAll :: XournalState 
                 -> HXournalState 
                 -> HXournalState
updatePageAll xst xstate = 
  let cmap = get canvasInfoMap xstate
      cmap' = fmap (updatePage xst . adjustPage xst) cmap
  in  set canvasInfoMap cmap' xstate 

adjustPage :: XournalState -> CanvasInfo -> CanvasInfo
adjustPage xojstate cinfo =
    let cpn = get currentPageNum cinfo 
        pagemap = case xojstate of 
                    ViewAppendState xoj -> get g_pages xoj 
                    SelectState txoj -> get g_selectAll txoj 
    in  adjustwork cpn pagemap              
  where adjustwork cpn pagemap = 
         if M.notMember cpn pagemap  
         then let (minp,_) = M.findMin pagemap 
                  (maxp,_) = M.findMax pagemap 
              in if cpn > maxp 
                   then set currentPageNum maxp cinfo
                   else set currentPageNum minp cinfo
         else cinfo
 


getPageFromXojBBoxMapPDF :: Int -> TXournalBBoxMapPDF -> TPageBBoxMapPDF
getPageFromXojBBoxMapPDF pagenum xoj  = 
  case M.lookup pagenum (gpages xoj) of 
    Nothing -> error "something wrong in getPageFromXojBBoxMapPDF"
    Just p -> p

updatePage :: XournalState -> CanvasInfo -> CanvasInfo 
updatePage (ViewAppendState xojbbox) cinfo = 
  let pagenum = get currentPageNum cinfo 
      pg = getPageFromXojBBoxMapPDF pagenum xojbbox 
      Dim w h = gdimension pg
  in  set currentPageNum pagenum 
      . set (pageDimension.viewInfo) (w,h)       
      . set currentPage (Left pg)
      $ cinfo 
updatePage (SelectState txoj) cinfo = 
  let pagenum = get currentPageNum cinfo
      mspage = gselectSelected txoj 
      pageFromArg = case M.lookup pagenum (gselectAll txoj) of 
                      Nothing -> error "no such page in updatePage"
                      Just p -> p
      (newpage,Dim w h) = 
        case mspage of 
          Nothing -> (Left pageFromArg, gdimension pageFromArg)
          Just (spagenum,page) -> 
            if spagenum == pagenum 
              then (Right page, gdimension page) 
              else (Left pageFromArg, gdimension pageFromArg)
  in set currentPageNum pagenum 
     . set (pageDimension.viewInfo) (w,h)       
     . set currentPage newpage
     $ cinfo 
  
setPage :: XournalState -> Int -> CanvasInfo -> CanvasInfo
setPage (ViewAppendState xojbbox) pagenum cinfo = 
  let pg = getPageFromXojBBoxMapPDF pagenum xojbbox 
      Dim w h = gdimension pg
  in  set currentPageNum pagenum 
      . set (viewPortOrigin.viewInfo) (0,0) 
      . set (pageDimension.viewInfo) (w,h)       
      . set currentPage (Left pg)
      $ cinfo 
setPage (SelectState txoj) pagenum cinfo = 
  let mspage = gselectSelected  txoj 
               -- tx_selectpage 
      pageFromArg = case M.lookup pagenum (gselectAll txoj) of 
                      Nothing -> error "no such page in setPage"
                      Just p -> p
      (newpage,Dim w h) = 
        case mspage of 
          Nothing -> (Left pageFromArg, gdimension pageFromArg)
          Just (spagenum,page) -> 
            if spagenum == pagenum 
              then (Right page, gdimension page) 
              else (Left pageFromArg, gdimension pageFromArg)
  in set currentPageNum pagenum 
     . set (viewPortOrigin.viewInfo) (0,0) 
     . set (pageDimension.viewInfo) (w,h)       
     . set currentPage newpage
     $ cinfo 
                            
getPage :: CanvasInfo -> TPageBBoxMapPDF
getPage cinfo = case get currentPage cinfo of 
                  Right tpgs -> tpageBBoxMapPDFFromTTempPageSelectPDF tpgs
                    
                    -- pageBBoxMapFromTempPageSelect tpgs
                  Left pg -> pg 
                  

