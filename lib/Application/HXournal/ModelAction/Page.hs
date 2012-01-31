{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.ModelAction.Page 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.ModelAction.Page where

import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.PageArrangement
import Application.HXournal.Type.Alias
import Application.HXournal.View.Coordinate
import Application.HXournal.Util
import Data.Xournal.BBox (moveBBoxToOrigin)
import Data.Xournal.Simple (Dimension(..))
import Data.Xournal.Generic
import Data.Xournal.Select

import Data.Traversable (mapM)

import Graphics.Xournal.Render.BBoxMapPDF

import Control.Category
import Data.Label
import Prelude hiding ((.),id,mapM)
import qualified Data.IntMap as M 

import Graphics.UI.Gtk (adjustmentSetUpper)
-- | 

getPage :: (ViewMode a) => CanvasInfo a -> (Page EditMode)
getPage = either id (gcast :: Page SelectMode -> Page EditMode) . get currentPage

-- |

getPageMap :: XournalState -> M.IntMap (Page EditMode)
getPageMap = either (get g_pages) (get g_selectAll) . xojstateEither 
  
-- |             
             
setPageMap :: M.IntMap (Page EditMode) -> XournalState -> XournalState
setPageMap nmap = 
  either (ViewAppendState . set g_pages nmap)
         (SelectState . set g_selectSelected Nothing . set g_selectAll nmap )
  . xojstateEither
  
        
updatePageFromCanvasToXournal :: (ViewMode a) => CanvasInfo a -> XournalState -> XournalState 
updatePageFromCanvasToXournal cinfo xojstate = 
  let cpn = get currentPageNum cinfo 
      epg = get currentPage cinfo
      page = either id gcast epg
  in  setPageMap (M.adjust (const page) cpn . getPageMap $ xojstate) xojstate 


updatePageAll :: XournalState 
                 -> HXournalState 
                 -> IO HXournalState
updatePageAll xst xstate = do 
  let cmap = get canvasInfoMap xstate
  putStrLn "updatePageAll" 
  cmap' <- mapM (updatePage xst . adjustPage xst) cmap
  putStrLn "updatePageAll2"
  return (set canvasInfoMap cmap' xstate)

adjustPage :: XournalState -> CanvasInfoBox -> CanvasInfoBox  
adjustPage xojstate = selectBox fsingle fsingle  
  where fsingle :: CanvasInfo a -> CanvasInfo a 
        fsingle cinfo = let cpn = get currentPageNum cinfo 
                            pagemap = getPageMap xojstate
                        in  adjustwork cpn pagemap              
          where adjustwork cpn pagemap = 
                  if M.notMember cpn pagemap  
                  then let (minp,_) = M.findMin pagemap 
                           (maxp,_) = M.findMax pagemap 
                       in if cpn > maxp 
                          then set currentPageNum maxp cinfo
                          else set currentPageNum minp cinfo
                  else cinfo
 


getPageFromGXournalMap :: Int -> GXournal M.IntMap a -> a
getPageFromGXournalMap pagenum = 
  maybeError "getPageFromGXournalMap" . M.lookup pagenum . get g_pages


-- | 

updateCvsInfoFrmXoj :: Xournal EditMode -> CanvasInfoBox -> IO CanvasInfoBox
updateCvsInfoFrmXoj xoj cinfobox = selectBoxAction fsingle fcont cinfobox
  where fsingle cinfo = do 
          let pagenum = get currentPageNum cinfo 
              page = getPage cinfo 
          let oarr = get (pageArrangement.viewInfo) cinfo 
              canvas = get drawArea cinfo 
              zmode = get (zoomMode.viewInfo) cinfo
          geometry <- makeCanvasGeometry EditMode (PageNum pagenum,page) 
                                         oarr canvas
          let cdim = canvasDim geometry 
              pg = getPageFromGXournalMap pagenum xoj 
              pdim@(PageDimension (Dim w h)) = PageDimension $ get g_dimension pg
              arr = makeSingleArrangement zmode pdim cdim 
              
              (hadj,vadj) = get adjustments cinfo
          adjustmentSetUpper hadj w 
          adjustmentSetUpper vadj h 
          -- adjustmentSetValue hadj 0
          -- adjustmentSetValue vadj 0
          
          return . CanvasInfoBox 
                 . set currentPageNum pagenum  
                 . set (pageArrangement.viewInfo) arr
                 . set currentPage (Left pg) $ cinfo
        fcont cinfo = do 
          let pagenum = get currentPageNum cinfo 
              page = getPage cinfo 
          let oarr = get (pageArrangement.viewInfo) cinfo 
              canvas = get drawArea cinfo 
              zmode = get (zoomMode.viewInfo) cinfo
          geometry <- makeCanvasGeometry EditMode (PageNum pagenum,page)
                                         oarr canvas
          let cdim = canvasDim geometry 
              pg = getPageFromGXournalMap pagenum xoj 
              pdim = PageDimension $ get g_dimension pg
              arr = makeContinuousSingleArrangement zmode cdim xoj (PageNum pagenum) 
              (hadj,vadj) = get adjustments cinfo
              ContinuousSingleArrangement (DesktopDimension (Dim w h)) _ _ = arr  
          adjustmentSetUpper hadj w 
          adjustmentSetUpper vadj h 
          
          return . CanvasInfoBox
                 . set currentPageNum pagenum  
                 . set (pageArrangement.viewInfo) arr
                 . set currentPage (Left pg) $ cinfo 

              
          {-
          ngeom <- makeCanvasGeometry EditMode (PageNum pagenum,page) arr canvas
          let DesktopDimension (Dim w h) = desktopDim ngeom          

          --  
          let plst = gToList . get g_pages $ xoj
              Dim _ h2 = get g_dimension (last plst)    
              
          let ddim = deskDimContSingle xoj 
              PageOrigin (_,h') = maybeError "test" $ pageArrFuncContSingle xoj (PageNum . (\x->x-1) . length $ plst )
          putStrLn (show (maximum (map (dim_width . get g_dimension) plst)))
          putStrLn (show h') --  (show h2) -- (show ddim)

          -- putStrLn $ "updateCvsInfoFrm..." ++ show w ++ " : " ++ show h
              
          -- putStrLn "updateCvsInfoFrm...2" 
          -}            

  

{-
-- | update page when single page view mode

updatePageViewMode :: (ViewMode a) => XournalState 
                      -> CanvasInfo a -> CanvasInfo SinglePage
updatePageViewMode (ViewAppendState xojbbox) cinfo = 
  let pagenum = get currentPageNum cinfo 
      pg = getPageFromGXournalMap pagenum xojbbox 
      Dim w h = gdimension pg
  in  set currentPageNum pagenum 
      . set (pageDimension.pageArrangement.viewInfo) (PageDimension (Dim w h))
      . set currentPage (Left pg)
      $ cinfo 
updatePageViewMode (SelectState txoj) cinfo = 
  let pagenum = get currentPageNum cinfo
      mspage = gselectSelected txoj 
      pageFromArg = maybeError "undatePageSingle" $ M.lookup pagenum (get g_selectAll txoj) 
      (newpage,Dim w h) = 
        case mspage of 
          Nothing -> (Left pageFromArg, gdimension pageFromArg)
          Just (spagenum,page) -> 
            if spagenum == pagenum 
              then (Right page, gdimension page) 
              else (Left pageFromArg, gdimension pageFromArg)
  in set currentPageNum pagenum 
     . set (pageDimension.pageArrangement.viewInfo) (PageDimension (Dim w h))
     . set currentPage newpage
     $ cinfo 
-}

-- |
updatePage :: XournalState -> CanvasInfoBox -> IO CanvasInfoBox 
updatePage = updateCvsInfoFrmXoj . either id makexoj . xojstateEither  
  where makexoj txoj = GXournal (get g_selectTitle txoj) (get g_selectAll txoj)

-- | 

setPage :: XournalState -> Int -> CanvasInfo a -> CanvasInfo a 
setPage xstate pagenum = 
  viewModeBranch (setPageSingle xstate pagenum) (error "setPage")    



-- | setPageSingle : in Single Page mode   

setPageSingle :: XournalState -> Int 
              -> CanvasInfo SinglePage 
              -> CanvasInfo SinglePage
setPageSingle (ViewAppendState xojbbox) pagenum cinfo = 
  let pg = getPageFromGXournalMap pagenum xojbbox
      Dim w h = get g_dimension pg
  in  set currentPageNum pagenum 
      . modify (viewPortBBox.pageArrangement.viewInfo) (apply moveBBoxToOrigin)
      . set (pageDimension.pageArrangement.viewInfo) (PageDimension (Dim w h))
      . set currentPage (Left pg)
      $ cinfo 
setPageSingle (SelectState txoj) pagenum cinfo = 
  let mspage = gselectSelected  txoj 
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
     . modify (viewPortBBox.pageArrangement.viewInfo) (apply moveBBoxToOrigin)
     . set (pageDimension.pageArrangement.viewInfo) (PageDimension (Dim w h))
     . set currentPage newpage
     $ cinfo 


-- | 

newSinglePageFromOld :: Page EditMode -> Page EditMode 
newSinglePageFromOld = 
  set g_layers (NoSelect [GLayerBuf (LyBuf Nothing) []]) 

-- | 

newPageBeforeAction :: (ViewMode a) => 
                       Xournal EditMode
                    -> (CanvasId, CanvasInfo a) 
                    -> IO (Xournal EditMode)
newPageBeforeAction xoj (_cid,cinfo) = do 
  let cpn = get currentPageNum cinfo
  let pagelst = M.elems . get g_pages $ xoj 
      pagekeylst = M.keys . get g_pages $ xoj 
      (pagesbefore,pagesafter) = splitAt cpn pagelst
      npage = newSinglePageFromOld (head pagesafter)
      npagelst = pagesbefore ++ (npage : pagesafter)
      nxoj = set g_pages (M.fromList . zip [0..] $ npagelst) xoj 
  putStrLn . show $ pagekeylst 
  return nxoj



{-
  case xojstate of 
    ViewAppendState xoj -> ViewAppendState (set g_pages nmap xoj)
    SelectState txoj -> let ntxoj = set g_selectSelected Nothing
                                    . set g_selectAll nmap 
                                    $ txoj 
                        in SelectState ntxoj
   -- SelectState (set g_selectAll nmap txoj) -}
