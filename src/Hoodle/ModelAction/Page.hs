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
import           Data.Traversable (mapM)
import           Graphics.UI.Gtk (adjustmentGetValue)
-- from hoodle-platform
import           Data.Hoodle.Generic
import           Data.Hoodle.Select
import           Graphics.Hoodle.Render.Type
-- from this package
import           Hoodle.Util
import           Hoodle.Type.Alias
import           Hoodle.Type.Canvas
import           Hoodle.Type.Enum
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.Predefined
import           Hoodle.View.Coordinate
-- 
import           Prelude hiding ((.),id,mapM)


-- |
getPageMap :: HoodleModeState -> M.IntMap (Page EditMode)
getPageMap = either (view gpages) (view gselAll) . hoodleModeStateEither 
  
-- |             
setPageMap :: M.IntMap (Page EditMode) -> HoodleModeState -> HoodleModeState
setPageMap nmap = 
  either (ViewAppendState . set gpages nmap)
         (SelectState . set gselSelected Nothing . set gselAll nmap )
  . hoodleModeStateEither
  
-- |
updatePageAll :: HoodleModeState -> HoodleState -> IO HoodleState
updatePageAll hdlmodst xstate = do 
  let cmap = getCanvasInfoMap xstate
  cmap' <- mapM (updatePage hdlmodst . adjustPage hdlmodst) cmap
  return $ maybe xstate id 
           . setCanvasInfoMap cmap' 
           . set hoodleModeState hdlmodst $ xstate

-- | 
adjustPage :: HoodleModeState -> CanvasInfoBox -> CanvasInfoBox  
adjustPage hdlmodst = selectBox fsingle fsingle  
  where fsingle :: CanvasInfo a -> CanvasInfo a 
        fsingle cinfo = let cpn = view currentPageNum cinfo 
                            pagemap = getPageMap hdlmodst
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
getPageFromGHoodleMap :: Int -> GHoodle M.IntMap a -> a
getPageFromGHoodleMap pagenum = 
  maybeError ("getPageFromGHoodleMap " ++ show pagenum) . M.lookup pagenum . view gpages


-- | 
updateCvsInfoFrmHoodle :: Hoodle EditMode -> CanvasInfoBox -> IO CanvasInfoBox
updateCvsInfoFrmHoodle hdl (CanvasSinglePage cinfo) = do
    let pagenum = view currentPageNum cinfo 
        oarr = view (viewInfo.pageArrangement) cinfo 
        canvas = view drawArea cinfo 
        zmode = view (viewInfo.zoomMode) cinfo
    geometry <- makeCanvasGeometry (PageNum pagenum) oarr canvas
    let cdim = canvasDim geometry 
        pg = getPageFromGHoodleMap pagenum hdl 
        pdim = PageDimension $ view gdimension pg
        (hadj,vadj) = view adjustments cinfo
    (xpos,ypos) <- (,) <$> adjustmentGetValue hadj <*> adjustmentGetValue vadj 
    let arr = makeSingleArrangement zmode pdim cdim (xpos,ypos)
        vinfo = view viewInfo cinfo 
        nvinfo = xfrmViewInfo (const arr) vinfo
    return 
      . CanvasSinglePage 
      . set currentPageNum pagenum
      . xfrmCvsInfo (const nvinfo) $ cinfo 
    -- return . CanvasInfoBox 
    --   . set currentPageNum pagenum  
    --   . set (viewInfo.pageArrangement) arr $ cinfo
updateCvsInfoFrmHoodle hdl (CanvasContPage cinfo) = do         
    let pagenum = view currentPageNum cinfo 
        oarr = view (viewInfo.pageArrangement) cinfo 
        canvas = view drawArea cinfo 
        zmode = view (viewInfo.zoomMode) cinfo
        (hadj,vadj) = view adjustments cinfo
    (xdesk,ydesk) <- (,) <$> adjustmentGetValue hadj 
                         <*> adjustmentGetValue vadj 
    geometry <- makeCanvasGeometry (PageNum pagenum) oarr canvas 
    case desktop2Page geometry (DeskCoord (xdesk,ydesk)) of
      Nothing -> return (CanvasContPage cinfo)
      Just ulcoord -> do 
        let cdim = canvasDim geometry 
        let arr = makeContinuousArrangement zmode cdim hdl ulcoord 
            vinfo = view viewInfo cinfo 
            nvinfo = xfrmViewInfo (const arr) vinfo
        return 
          . CanvasContPage 
          . set currentPageNum pagenum
          . xfrmCvsInfo (const nvinfo) $ cinfo 

   
--     let ulcoord = maybeError "updateCvsFromHoodle" $ 
            

-- |
updatePage :: HoodleModeState -> CanvasInfoBox -> IO CanvasInfoBox 
updatePage (ViewAppendState hdl) c = updateCvsInfoFrmHoodle hdl c
updatePage (SelectState thdl) c = do 
    let hdl = GHoodle (view gselTitle thdl) (view gselAll thdl)
    updateCvsInfoFrmHoodle hdl c

--     boxAction f cinfobox
--  where f :: (ViewMode a) => ViewModeSumType -> CanvasInfo a -> IO CanvasInfoBox
--         f mode cinfo = do 
--        fcont _cinfo = do 
--          let hdl = GHoodle (view gselTitle thdl) (view gselAll thdl)
--          updateCvsInfoFrmHoodle VMContPage hdl cinfobox

-- | 
setPage :: HoodleState -> PageNum -> CanvasId -> IO CanvasInfoBox
setPage xstate pnum cid = do  
  let cinfobox =  getCanvasInfo cid xstate
  selectBoxAction (liftM CanvasSinglePage . setPageSingle xstate pnum) 
                  (liftM CanvasContPage . setPageCont xstate pnum)
                  cinfobox

-- | setPageSingle : in Single Page mode   
setPageSingle :: HoodleState -> PageNum  
              -> CanvasInfo SinglePage
              -> IO (CanvasInfo SinglePage)
setPageSingle xstate pnum cinfo = do 
  let hdl = getHoodle xstate
  geometry <- getCvsGeomFrmCvsInfo cinfo
  let cdim = canvasDim geometry 
  let pg = getPageFromGHoodleMap (unPageNum pnum) hdl
      pdim = PageDimension (view gdimension pg)
      zmode = view (viewInfo.zoomMode) cinfo
      arr = makeSingleArrangement zmode pdim cdim (0,0)  
  return $ set currentPageNum (unPageNum pnum)
           . set (viewInfo.pageArrangement) arr $ cinfo 

-- | setPageCont : in Continuous Page mode   
setPageCont :: HoodleState -> PageNum  
            -> CanvasInfo ContinuousPage
            -> IO (CanvasInfo ContinuousPage)
setPageCont xstate pnum cinfo = do 
  let hdl = getHoodle xstate
  geometry <- getCvsGeomFrmCvsInfo cinfo
  let cdim = canvasDim geometry 
      zmode = view (viewInfo.zoomMode) cinfo
      arr = makeContinuousArrangement zmode cdim hdl (pnum,PageCoord (0,0))  
  return $ set currentPageNum (unPageNum pnum)
           . set (viewInfo.pageArrangement) arr $ cinfo 

-- | 
newSinglePageFromOld :: Page EditMode -> Page EditMode 
newSinglePageFromOld = set glayers (fromList [emptyRLayer])
  
  -- (NoSelect [emptyRLayer]) 


-- | 
addNewPageInHoodle :: AddDirection  
                   -> Hoodle EditMode
                   -> Int 
                   -> IO (Hoodle EditMode)
addNewPageInHoodle dir hdl cpn = do 
  let pagelst = M.elems . view gpages $ hdl
      (pagesbefore,cpage:pagesafter) = splitAt cpn pagelst
      npage = newSinglePageFromOld cpage
      npagelst = case dir of 
                   PageBefore -> pagesbefore ++ (npage : cpage : pagesafter)
                   PageAfter -> pagesbefore ++ (cpage : npage : pagesafter)
      nhdl = set gpages (M.fromList . zip [0..] $ npagelst) hdl
  return nhdl

-- | 
relZoomRatio :: CanvasGeometry -> ZoomModeRel -> Double
relZoomRatio geometry rzmode =   
    let CvsCoord (cx0,_cy0) = desktop2Canvas geometry (DeskCoord (0,0))
        CvsCoord (cx1,_cy1) = desktop2Canvas geometry (DeskCoord (1,1))
        scalefactor = case rzmode of
          ZoomIn -> predefinedZoomStepFactor
          ZoomOut -> 1.0/predefinedZoomStepFactor
    in (cx1-cx0) * scalefactor
     
