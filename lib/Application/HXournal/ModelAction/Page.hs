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
  in set currentPage (Left pg) cinfo 
updatePage (SelectState xojselect) cinfo = 
  let pagenum = get currentPageNum cinfo 
  in case pages xojselect of
       Left pgs -> let pg = case M.lookup pagenum pgs of
                              Nothing -> error "error in SetPage"
                              Just p -> p
                   in  set currentPage (Left pg) cinfo 
       Right _ -> error "not yet defined here"
  

setPage :: XournalState -> Int -> CanvasInfo -> CanvasInfo
setPage (ViewAppendState xojbbox) pagenum cinfo = 
  let pg = getPageFromXojBBoxMap pagenum xojbbox 
      Dim w h = pageDim pg
  in  set currentPageNum pagenum 
      . set (viewPortOrigin.viewInfo) (0,0) 
      . set (pageDimension.viewInfo) (w,h)       
      . set currentPage (Left pg)
      $ cinfo 
setPage (SelectState xojselect) pagenum cinfo = 
  case pages xojselect of
    Left pgs -> let pg = case M.lookup pagenum pgs of
                           Nothing -> error "error in SetPage"
                           Just p -> p 
                    Dim w h = pageDim pg 
                in set currentPageNum pagenum 
                   . set (viewPortOrigin.viewInfo) (0,0) 
                   . set (pageDimension.viewInfo) (w,h)       
                   . set currentPage (Left pg)
                   $ cinfo 
    Right _ -> error "not yet defined here"


getPage :: CanvasInfo -> PageBBoxMap
getPage cinfo = case get currentPage cinfo of 
                  Right pgselect -> pageBBoxMapFromPageSelect $ pgselect
                  -- Right _ -> error "not implemented yet in getPage"
                  Left pg -> pg 
                  