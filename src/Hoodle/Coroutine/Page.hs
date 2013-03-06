{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Page 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Page where

import           Control.Lens (view,set,over)
import           Control.Monad
import           Control.Monad.State
import qualified Data.IntMap as M
-- from hoodle-platform
import           Data.Hoodle.BBox (BBox(..), moveBBoxULCornerTo)
import           Data.Hoodle.Generic
import           Data.Hoodle.Select
import qualified Data.Hoodle.Simple as S
import           Graphics.Hoodle.Render.Type.Background
-- from this package
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Commit
import           Hoodle.Coroutine.Scroll
import           Hoodle.ModelAction.Page
import           Hoodle.Type.Alias
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Canvas
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.HoodleState
import           Hoodle.Type.Enum
import           Hoodle.Util
import           Hoodle.View.Coordinate
import           Hoodle.View.Draw
-- 

-- | change page of current canvas using a modify function
changePage :: (Int -> Int) -> MainCoroutine () 
changePage modifyfn = updateXState changePageAction 
                      >> adjustScrollbarWithGeometryCurrent
                      >> invalidateAll -- invalidateCurrent
  where changePageAction xst = selectBoxAction (fsingle xst) (fcont xst) 
                               . view currentCanvasInfo $ xst
        fsingle xstate cvsInfo = do 
          let xojst = view hoodleModeState $ xstate  
              npgnum = modifyfn (view currentPageNum cvsInfo)
              cid = view canvasId cvsInfo
              bsty = view backgroundStyle xstate 
              (b,npgnum',_selectedpage,xojst') = changePageInHoodleModeState bsty npgnum xojst
          xstate' <- liftIO $ updatePageAll xojst' xstate 
          ncvsInfo <- liftIO $ setPage xstate' (PageNum npgnum') cid
          xstatefinal <- return . over currentCanvasInfo (const ncvsInfo) $ xstate'
          when b (commit xstatefinal)
          return xstatefinal 
        
        fcont xstate cvsInfo = do 
          let xojst = view hoodleModeState xstate  
              npgnum = modifyfn (view currentPageNum cvsInfo)
              cid = view canvasId cvsInfo
              bsty = view backgroundStyle xstate 
              (b,npgnum',_selectedpage,xojst') = changePageInHoodleModeState bsty npgnum xojst
          xstate' <- liftIO $ updatePageAll xojst' xstate 
          ncvsInfo <- liftIO $ setPage xstate' (PageNum npgnum') cid
          xstatefinal <- return . over currentCanvasInfo (const ncvsInfo) $ xstate'
          when b (commit xstatefinal)
          return xstatefinal 


-- | 
changePageInHoodleModeState :: BackgroundStyle 
                            -> Int  -- ^ new page number 
                            -> HoodleModeState 
                            -> (Bool,Int,Page EditMode,HoodleModeState)
changePageInHoodleModeState bsty npgnum hdlmodst =
    let ehdl = hoodleModeStateEither hdlmodst 
        pgs = either (view gpages) (view gselAll) ehdl
        totnumpages = M.size pgs
        lpage = maybeError' "changePage" (M.lookup (totnumpages-1) pgs)
        (isChanged,npgnum',npage',ehdl') 
          | npgnum >= totnumpages = 
            let cbkg = view gbackground lpage
                nbkg 
                  | isRBkgSmpl cbkg = cbkg { rbkg_style = convertBackgroundStyleToByteString bsty } 
                  | otherwise = cbkg 
                npage = set gbackground nbkg 
                        . newSinglePageFromOld $ lpage
                npages = M.insert totnumpages npage pgs 
            in (True,totnumpages,npage,
                 either (Left . set gpages npages) (Right. set gselAll npages) ehdl )
          | otherwise = let npg = if npgnum < 0 then 0 else npgnum
                            pg = maybeError' "changePage" (M.lookup npg pgs)
                        in (False,npg,pg,ehdl) 
    in (isChanged,npgnum',npage',either ViewAppendState SelectState ehdl')


-- | 
canvasZoomUpdateGenRenderCvsId :: MainCoroutine () 
                                  -> CanvasId 
                                  -> Maybe ZoomMode 
                                  -> Maybe (PageNum,PageCoordinate) 
                                  -> MainCoroutine ()
canvasZoomUpdateGenRenderCvsId renderfunc cid mzmode mcoord 
  = updateXState zoomUpdateAction 
    >> adjustScrollbarWithGeometryCvsId cid
    >> renderfunc
  where zoomUpdateAction xst =  
          selectBoxAction (fsingle xst) (fcont xst) . getCanvasInfo cid $ xst 
        fsingle xstate cinfo = do   
          geometry <- liftIO $ getCvsGeomFrmCvsInfo cinfo 
          page <- getCurrentPageCvsId cid
          let zmode = maybe (view (viewInfo.zoomMode) cinfo) id mzmode  
              pdim = PageDimension $ view gdimension page
              xy = either (const (0,0)) (unPageCoord.snd) 
                     (getCvsOriginInPage geometry)
              cdim = canvasDim geometry 
              narr = makeSingleArrangement zmode pdim cdim xy  
              ncinfobox = CanvasSinglePage
                          . set (viewInfo.pageArrangement) narr
                          . set (viewInfo.zoomMode) zmode $ cinfo
          return . modifyCanvasInfo cid (const ncinfobox) $ xstate
        fcont xstate cinfo = do   
          geometry <- liftIO $ getCvsGeomFrmCvsInfo cinfo 
          let zmode = maybe (view (viewInfo.zoomMode) cinfo) id mzmode 
              cpn = PageNum $ view currentPageNum cinfo 
              cdim = canvasDim geometry 
              hdl = getHoodle xstate 
              origcoord = case mcoord of
                            Just coord -> coord 
                            Nothing -> either (const (cpn,PageCoord (0,0))) id 
                                         (getCvsOriginInPage geometry)
              narr = makeContinuousArrangement zmode cdim hdl origcoord
              ncinfobox = CanvasContPage
                          . set (viewInfo.pageArrangement) narr
                          . set (viewInfo.zoomMode) zmode $ cinfo
          return . modifyCanvasInfo cid (const ncinfobox) $ xstate

-- | 
canvasZoomUpdateCvsId :: CanvasId 
                         -> Maybe ZoomMode 
                         -> MainCoroutine ()
canvasZoomUpdateCvsId cid mzmode = 
  canvasZoomUpdateGenRenderCvsId invalidateAll cid mzmode Nothing
  
-- | 
canvasZoomUpdateBufAll :: MainCoroutine () 
canvasZoomUpdateBufAll = do 
    klst <- liftM (M.keys . getCanvasInfoMap) get
    mapM_ updatefunc klst 
  where 
    updatefunc cid 
      = canvasZoomUpdateGenRenderCvsId  (invalidateInBBox Nothing Efficient cid) cid Nothing Nothing 


-- |
canvasZoomUpdateAll :: MainCoroutine () 
canvasZoomUpdateAll = do 
  klst <- liftM (M.keys . getCanvasInfoMap) get
  mapM_ (flip canvasZoomUpdateCvsId Nothing) klst 


-- | 
canvasZoomUpdate :: Maybe ZoomMode -> MainCoroutine () 
canvasZoomUpdate mzmode = do  
  cid <- (liftM (getCurrentCanvasId) get)
  canvasZoomUpdateCvsId cid mzmode

-- |
pageZoomChange :: ZoomMode -> MainCoroutine () 
pageZoomChange = canvasZoomUpdate . Just 

-- | 
pageZoomChangeRel :: ZoomModeRel -> MainCoroutine () 
pageZoomChangeRel rzmode = do 
    boxAction fsingle . view currentCanvasInfo =<< get 
  where 
    fsingle :: (ViewMode a) => CanvasInfo a -> MainCoroutine ()
    fsingle cinfo = do 
      let cpn = PageNum (view currentPageNum cinfo)
          arr = view (viewInfo.pageArrangement) cinfo 
          canvas = view drawArea cinfo 
      geometry <- liftIO $ makeCanvasGeometry cpn arr canvas
      let  nratio = relZoomRatio geometry rzmode
      pageZoomChange (Zoom nratio)

-- |
newPage :: AddDirection -> MainCoroutine () 
newPage dir = updateXState npgBfrAct 
              >> commit_ 
              >> canvasZoomUpdateAll 
              >> invalidateAll
  where 
    npgBfrAct xst = boxAction (fsimple xst) . view currentCanvasInfo $ xst
    fsimple :: (ViewMode a) => HoodleState -> CanvasInfo a 
               -> MainCoroutine HoodleState
    fsimple xstate cinfo = do 
      case view hoodleModeState xstate of 
        ViewAppendState hdl -> do 
          let bsty = view backgroundStyle xstate 
              hdl' = addNewPageInHoodle bsty dir hdl (view currentPageNum cinfo)
          return =<< liftIO . updatePageAll (ViewAppendState hdl')
                     . set hoodleModeState  (ViewAppendState hdl') $ xstate 
        SelectState _ -> do 
          liftIO $ putStrLn " not implemented yet"
          return xstate
      
-- | delete current page of current canvas
deleteCurrentPage :: MainCoroutine ()           
deleteCurrentPage = do 
    updateXState delpgact >> commit_ >> canvasZoomUpdateAll >> invalidateAll
  where 
    delpgact xst = boxAction (fsimple xst) . view currentCanvasInfo $ xst
    fsimple :: (ViewMode a) => HoodleState -> CanvasInfo a
               -> MainCoroutine HoodleState
    fsimple xstate cinfo = do 
      case view hoodleModeState xstate of 
        ViewAppendState hdl -> do 
          hdl' <- liftIO $ deletePageInHoodle hdl 
                             (PageNum (view currentPageNum cinfo))
          return =<< liftIO . updatePageAll (ViewAppendState hdl')
                     . set hoodleModeState  (ViewAppendState hdl') $ xstate 
        SelectState _ -> do 
          liftIO $ putStrLn " not implemented yet"
          return xstate
      
-- | delete designated page
deletePageInHoodle :: Hoodle EditMode -> PageNum -> IO (Hoodle EditMode)
deletePageInHoodle hdl (PageNum pgn) = do 
  let pagelst = M.elems . view gpages $ hdl 
      (pagesbefore,_cpage:pagesafter) = splitAt pgn pagelst
      npagelst = pagesbefore ++ pagesafter
      nhdl = set gpages (M.fromList . zip [0..] $ npagelst) hdl
  return nhdl



