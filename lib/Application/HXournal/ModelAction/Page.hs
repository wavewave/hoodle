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

import Control.Applicative
import Control.Monad (liftM)
-- import Data.Xournal.BBox (moveBBoxToOrigin)
import Data.Xournal.Simple (Dimension(..))
import Data.Xournal.Generic
import Data.Xournal.Select

import Data.Traversable (mapM)

import Graphics.Xournal.Render.BBoxMapPDF

import Control.Category
import Data.Label
import Prelude hiding ((.),id,mapM)
import qualified Data.IntMap as M 

import Graphics.UI.Gtk (adjustmentSetUpper,adjustmentGetValue)

-- |

getPageMap :: XournalState -> M.IntMap (Page EditMode)
getPageMap = either (get g_pages) (get g_selectAll) . xojstateEither 
  
-- |             
             
setPageMap :: M.IntMap (Page EditMode) -> XournalState -> XournalState
setPageMap nmap = 
  either (ViewAppendState . set g_pages nmap)
         (SelectState . set g_selectSelected Nothing . set g_selectAll nmap )
  . xojstateEither
  
        
{-
updatePageFromCanvasToXournal :: (ViewMode a) => CanvasInfo a -> XournalState -> XournalState 
updatePageFromCanvasToXournal cinfo xojstate = 
  let cpn = get curentPageNum     
      page = getCurrentPageFromXojState cinfo xojstate 
 -- either id gcast epg
  in  setPageMap (M.adjust (const page) cpn . getPageMap $ xojstate) xojstate 

      -- epg = get currentPage cinfo
-}

-- |

updatePageAll :: XournalState -> HXournalState -> IO HXournalState
updatePageAll xojst xstate = do 
  let cmap = getCanvasInfoMap xstate
  cmap' <- mapM (updatePage xojst . adjustPage xojst) cmap
  return $ maybe xstate id 
           . setCanvasInfoMap cmap' 
           . set xournalstate xojst $ xstate

-- | 

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
  maybeError ("getPageFromGXournalMap " ++ show pagenum) . M.lookup pagenum . get g_pages


-- | 

updateCvsInfoFrmXoj :: Xournal EditMode -> CanvasInfoBox -> IO CanvasInfoBox
updateCvsInfoFrmXoj xoj cinfobox = selectBoxAction fsingle fcont cinfobox
  where fsingle cinfo = do 
          let pagenum = get currentPageNum cinfo 
          let oarr = get (pageArrangement.viewInfo) cinfo 
              canvas = get drawArea cinfo 
              zmode = get (zoomMode.viewInfo) cinfo
          geometry <- makeCanvasGeometry (PageNum pagenum) oarr canvas
          let cdim = canvasDim geometry 
              pg = getPageFromGXournalMap pagenum xoj 
              pdim@(PageDimension (Dim w h)) = PageDimension $ get g_dimension pg
              (hadj,vadj) = get adjustments cinfo
          (xpos,ypos) <- (,) <$> adjustmentGetValue hadj <*> adjustmentGetValue vadj 
          let arr = makeSingleArrangement zmode pdim cdim (xpos,ypos)
          adjustmentSetUpper hadj w 
          adjustmentSetUpper vadj h 
          return . CanvasInfoBox 
                 . set currentPageNum pagenum  
                 . set (pageArrangement.viewInfo) arr $ cinfo
        fcont cinfo = do 
          let pagenum = get currentPageNum cinfo 
          let oarr = get (pageArrangement.viewInfo) cinfo 
              canvas = get drawArea cinfo 
              zmode = get (zoomMode.viewInfo) cinfo
              (hadj,vadj) = get adjustments cinfo
          (xdesk,ydesk) <- (,) <$> adjustmentGetValue hadj 
                               <*> adjustmentGetValue vadj 
          geometry <- makeCanvasGeometry (PageNum pagenum) oarr canvas 
          let ulcoord = maybeError "updateCvsFromXoj" $ 
                          desktop2Page geometry (DeskCoord (xdesk,ydesk))
          let cdim = canvasDim geometry 
              -- pg = getPageFromGXournalMap pagenum xoj 
              -- pdim = PageDimension $ get g_dimension pg
          let arr = makeContinuousSingleArrangement zmode cdim xoj ulcoord 
              ContinuousSingleArrangement _ (DesktopDimension (Dim w h)) _ _ = arr  
          adjustmentSetUpper hadj w 
          adjustmentSetUpper vadj h 
          return . CanvasInfoBox
                 . set currentPageNum pagenum  
                 . set (pageArrangement.viewInfo) arr $ cinfo 

              

-- |
{-
updatePage :: XournalState -> CanvasInfoBox -> IO CanvasInfoBox 
updatePage = either updateCvsInfoFrmXoj . either id makexoj . xojstateEither  
  where makexoj txoj = GXournal (get g_selectTitle txoj) (get g_selectAll txoj)
-}


updatePage :: XournalState -> CanvasInfoBox -> IO CanvasInfoBox 
updatePage (ViewAppendState xojbbox) cinfobox = updateCvsInfoFrmXoj xojbbox cinfobox
updatePage (SelectState txoj) cinfobox = selectBoxAction fsingle fcont cinfobox
  where fsingle _cinfo = do 
          let xoj = GXournal (get g_selectTitle txoj) (get g_selectAll txoj)
          updateCvsInfoFrmXoj xoj cinfobox
        fcont _cinfo = do 
          let xoj = GXournal (get g_selectTitle txoj) (get g_selectAll txoj)
          updateCvsInfoFrmXoj xoj cinfobox

  
  
 {- 
  
-- getselectedpage :: CanvasInfo a -> Either (Page EditMode) (Page SelectMode)
        -- getselectedpage cinfo = 
        --   let pagenum = get currentPageNum cinfo 
        --       pgs = get g_selectAll txoj
        --     pg = maybeError "??" (M.lookup pagenum pgs)
        --       spg = case get g_selectSelected txoj of 
        --              Nothing -> Left pg 
        --              Just (spnum,tpg) -> if spnum == pagenum then Right tpg else Left pg
        --   in spg


  
  let pagenum = unboxGet currentPageNum cinfobox
      mspage = get g_selectSelected txoj 
      pageFromArg = case M.lookup pagenum (get g_selectAll txoj) of 
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

-}

-- | 

setPage :: HXournalState -> PageNum -> CanvasId -> IO CanvasInfoBox
setPage xstate pnum cid = do  
  let cinfobox =  getCanvasInfo cid xstate
  selectBoxAction (liftM CanvasInfoBox . setPageSingle xstate pnum) 
                  (liftM CanvasInfoBox . setPageCont xstate pnum)
                  cinfobox

-- | setPageSingle : in Single Page mode   

setPageSingle :: HXournalState -> PageNum  
              -> CanvasInfo SinglePage
              -> IO (CanvasInfo SinglePage)
setPageSingle xstate pnum cinfo = do 
  let xoj = getXournal xstate
  geometry <- getCvsGeomFrmCvsInfo cinfo
  let cdim = canvasDim geometry 
  let pg = getPageFromGXournalMap (unPageNum pnum) xoj
      pdim = PageDimension (get g_dimension pg)
      zmode = get (zoomMode.viewInfo) cinfo
      arr = makeSingleArrangement zmode pdim cdim (0,0)  
  return $ set currentPageNum (unPageNum pnum)
           . set (pageArrangement.viewInfo) arr $ cinfo 


-- | setPageCont : in ContinuousSingle Page mode   

setPageCont :: HXournalState -> PageNum  
            -> CanvasInfo ContinuousSinglePage
            -> IO (CanvasInfo ContinuousSinglePage)
setPageCont xstate pnum cinfo = do 
  let xoj = getXournal xstate
  geometry <- getCvsGeomFrmCvsInfo cinfo
  let cdim = canvasDim geometry 
  let -- pg = getPageFromGXournalMap (unPageNum pnum) xoj
      zmode = get (zoomMode.viewInfo) cinfo
      arr = makeContinuousSingleArrangement zmode cdim xoj (pnum,PageCoord (0,0))  
  return $ set currentPageNum (unPageNum pnum)
           . set (pageArrangement.viewInfo) arr $ cinfo 



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
      -- pagekeylst = M.keys . get g_pages $ xoj 
      (pagesbefore,pagesafter) = splitAt cpn pagelst
      npage = newSinglePageFromOld (head pagesafter)
      npagelst = pagesbefore ++ (npage : pagesafter)
      nxoj = set g_pages (M.fromList . zip [0..] $ npagelst) xoj 
  return nxoj

