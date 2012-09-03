{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.ModelAction.Page 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.ModelAction.Page where

import           Control.Applicative
import           Control.Category
import           Control.Lens
import           Control.Monad (liftM)
import qualified Data.IntMap as M
-- import           Data.Label
import           Data.Traversable (mapM)
import           Graphics.UI.Gtk (adjustmentGetValue)
-- from hoodle-platform
import           Data.Xournal.Generic
import           Data.Xournal.Select
import           Graphics.Xournal.Render.BBoxMapPDF
-- from this package
import           Hoodle.Type.XournalState
import           Hoodle.Type.Canvas
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.Alias
import           Hoodle.Type.Enum
import           Hoodle.Type.Predefined
import           Hoodle.View.Coordinate
import           Hoodle.Util
-- 
import           Prelude hiding ((.),id,mapM)


-- |
getPageMap :: XournalState -> M.IntMap (Page EditMode)
getPageMap = either (view g_pages) (view g_selectAll) . xojstateEither 
  
-- |             
setPageMap :: M.IntMap (Page EditMode) -> XournalState -> XournalState
setPageMap nmap = 
  either (ViewAppendState . set g_pages nmap)
         (SelectState . set g_selectSelected Nothing . set g_selectAll nmap )
  . xojstateEither
  
-- |
updatePageAll :: XournalState -> HoodleState -> IO HoodleState
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
        fsingle cinfo = let cpn = view currentPageNum cinfo 
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
 
-- | 
getPageFromGXournalMap :: Int -> GXournal M.IntMap a -> a
getPageFromGXournalMap pagenum = 
  maybeError ("getPageFromGXournalMap " ++ show pagenum) . M.lookup pagenum . view g_pages


-- | 
updateCvsInfoFrmXoj :: Xournal EditMode -> CanvasInfoBox -> IO CanvasInfoBox
updateCvsInfoFrmXoj xoj cinfobox = selectBoxAction fsingle fcont cinfobox
  where fsingle cinfo = do 
          let pagenum = view currentPageNum cinfo 
          let oarr = view (viewInfo.pageArrangement) cinfo 
              canvas = view drawArea cinfo 
              zmode = view (viewInfo.zoomMode) cinfo
          geometry <- makeCanvasGeometry (PageNum pagenum) oarr canvas
          let cdim = canvasDim geometry 
              pg = getPageFromGXournalMap pagenum xoj 
              pdim = PageDimension $ view g_dimension pg
              (hadj,vadj) = view adjustments cinfo
          (xpos,ypos) <- (,) <$> adjustmentGetValue hadj <*> adjustmentGetValue vadj 
          let arr = makeSingleArrangement zmode pdim cdim (xpos,ypos)
          return . CanvasInfoBox 
                 . set currentPageNum pagenum  
                 . set (viewInfo.pageArrangement) arr $ cinfo
        fcont cinfo = do 
          let pagenum = view currentPageNum cinfo 
          let oarr = view (viewInfo.pageArrangement) cinfo 
              canvas = view drawArea cinfo 
              zmode = view (viewInfo.zoomMode) cinfo
              (hadj,vadj) = view adjustments cinfo
          (xdesk,ydesk) <- (,) <$> adjustmentGetValue hadj 
                               <*> adjustmentGetValue vadj 
          geometry <- makeCanvasGeometry (PageNum pagenum) oarr canvas 
          let ulcoord = maybeError "updateCvsFromXoj" $ 
                          desktop2Page geometry (DeskCoord (xdesk,ydesk))
          let cdim = canvasDim geometry 
          let arr = makeContinuousSingleArrangement zmode cdim xoj ulcoord 
          return . CanvasInfoBox
                 . set currentPageNum pagenum  
                 . set (viewInfo.pageArrangement) arr $ cinfo 

              

-- |
updatePage :: XournalState -> CanvasInfoBox -> IO CanvasInfoBox 
updatePage (ViewAppendState xojbbox) cinfobox = updateCvsInfoFrmXoj xojbbox cinfobox
updatePage (SelectState txoj) cinfobox = selectBoxAction fsingle fcont cinfobox
  where fsingle _cinfo = do 
          let xoj = GXournal (view g_selectTitle txoj) (view g_selectAll txoj)
          updateCvsInfoFrmXoj xoj cinfobox
        fcont _cinfo = do 
          let xoj = GXournal (view g_selectTitle txoj) (view g_selectAll txoj)
          updateCvsInfoFrmXoj xoj cinfobox

-- | 
setPage :: HoodleState -> PageNum -> CanvasId -> IO CanvasInfoBox
setPage xstate pnum cid = do  
  let cinfobox =  getCanvasInfo cid xstate
  selectBoxAction (liftM CanvasInfoBox . setPageSingle xstate pnum) 
                  (liftM CanvasInfoBox . setPageCont xstate pnum)
                  cinfobox

-- | setPageSingle : in Single Page mode   
setPageSingle :: HoodleState -> PageNum  
              -> CanvasInfo SinglePage
              -> IO (CanvasInfo SinglePage)
setPageSingle xstate pnum cinfo = do 
  let xoj = getXournal xstate
  geometry <- getCvsGeomFrmCvsInfo cinfo
  let cdim = canvasDim geometry 
  let pg = getPageFromGXournalMap (unPageNum pnum) xoj
      pdim = PageDimension (view g_dimension pg)
      zmode = view (viewInfo.zoomMode) cinfo
      arr = makeSingleArrangement zmode pdim cdim (0,0)  
  return $ set currentPageNum (unPageNum pnum)
           . set (viewInfo.pageArrangement) arr $ cinfo 

-- | setPageCont : in ContinuousSingle Page mode   
setPageCont :: HoodleState -> PageNum  
            -> CanvasInfo ContinuousSinglePage
            -> IO (CanvasInfo ContinuousSinglePage)
setPageCont xstate pnum cinfo = do 
  let xoj = getXournal xstate
  geometry <- getCvsGeomFrmCvsInfo cinfo
  let cdim = canvasDim geometry 
      zmode = view (viewInfo.zoomMode) cinfo
      arr = makeContinuousSingleArrangement zmode cdim xoj (pnum,PageCoord (0,0))  
  return $ set currentPageNum (unPageNum pnum)
           . set (viewInfo.pageArrangement) arr $ cinfo 

-- | 
newSinglePageFromOld :: Page EditMode -> Page EditMode 
newSinglePageFromOld = 
  set g_layers (NoSelect [GLayerBuf (LyBuf Nothing) []]) 


-- | 
addNewPageInXoj :: AddDirection  
                   -> Xournal EditMode
                   -> Int 
                   -> IO (Xournal EditMode)
addNewPageInXoj dir xoj cpn = do 
  let pagelst = M.elems . view g_pages $ xoj 
      (pagesbefore,cpage:pagesafter) = splitAt cpn pagelst
      npage = newSinglePageFromOld cpage
      npagelst = case dir of 
                   PageBefore -> pagesbefore ++ (npage : cpage : pagesafter)
                   PageAfter -> pagesbefore ++ (cpage : npage : pagesafter)
      nxoj = set g_pages (M.fromList . zip [0..] $ npagelst) xoj 
  return nxoj

-- | 
relZoomRatio :: CanvasGeometry -> ZoomModeRel -> Double
relZoomRatio geometry rzmode =   
    let CvsCoord (cx0,_cy0) = desktop2Canvas geometry (DeskCoord (0,0))
        CvsCoord (cx1,_cy1) = desktop2Canvas geometry (DeskCoord (1,1))
        scalefactor = case rzmode of
          ZoomIn -> predefinedZoomStepFactor
          ZoomOut -> 1.0/predefinedZoomStepFactor
    in (cx1-cx0) * scalefactor
     
