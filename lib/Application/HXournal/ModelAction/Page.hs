module Application.HXournal.ModelAction.Page where

import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Canvas

import Text.Xournal.Type
import Graphics.Xournal.Type
import Graphics.Xournal.Type.Select

import Control.Category
import Data.Label
import Prelude hiding ((.),id)

updatePage :: XournalState -> CanvasInfo -> CanvasInfo 
updatePage (ViewAppendState xojbbox) cinfo = 
  let pgs = xournalPages xojbbox 
      pagenum = get currentPageNum cinfo 
      pg = pgs !! pagenum 
  in set currentPage (Left pg) cinfo 
updatePage (SelectState xojselect) cinfo = 
  let pagenum = get currentPageNum cinfo 
  in case pages xojselect of
       Left pgs -> let pg = pgs !! pagenum 
                   in  set currentPage (Left pg) cinfo 
       Right _ -> error "not yet defined here"
  

setPage :: XournalState -> Int -> CanvasInfo -> CanvasInfo
setPage (ViewAppendState xojbbox) pagenum cinfo = 
  let pgs = xournalPages xojbbox 
      pg = pgs !! pagenum 
      Dim w h = pageDim pg
  in  set currentPageNum pagenum 
      . set (viewPortOrigin.viewInfo) (0,0) 
      . set (pageDimension.viewInfo) (w,h)       
      . set currentPage (Left pg)
      $ cinfo 
setPage (SelectState xojselect) pagenum cinfo = 
  case pages xojselect of
    Left pgs -> let pg = pgs !! pagenum 
                    Dim w h = pageDim pg 
                in set currentPageNum pagenum 
                   . set (viewPortOrigin.viewInfo) (0,0) 
                   . set (pageDimension.viewInfo) (w,h)       
                   . set currentPage (Left pg)
                   $ cinfo 
    Right _ -> error "not yet defined here"

getPage :: CanvasInfo -> PageBBox 
getPage cinfo = case get currentPage cinfo of 
                  Right pgselect -> pageBBoxFromPageSelect pgselect
                  Left pg -> pg 
                  