{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Coroutine.Page 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.Coroutine.Page where

import Control.Monad
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.PageArrangement
import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Enum
import Application.HXournal.Util
import Application.HXournal.View.Coordinate
import Application.HXournal.Accessor
import Application.HXournal.Coroutine.Draw
import Application.HXournal.Coroutine.Commit
import Application.HXournal.Coroutine.Scroll
import Application.HXournal.ModelAction.Page
import Application.HXournal.Type.Alias
import Data.Xournal.Generic
import Control.Monad.Trans
import Control.Category
import Data.Label
import Prelude hiding ((.), id)
import qualified Data.IntMap as M

-- | change page of current canvas using a modify function

changePage :: (Int -> Int) -> MainCoroutine () 
changePage modifyfn = updateXState changePageAction 
                      >> adjustScrollbarWithGeometryCurrent
                      >> invalidateCurrent
  where changePageAction xst = selectBoxAction (fsingle xst) (fcont xst) 
                               . get currentCanvasInfo $ xst
        fsingle xstate cvsInfo = do 
          let xojst = get xournalstate $ xstate  
              npgnum = modifyfn (get currentPageNum cvsInfo)
              cid = get canvasId cvsInfo
              (b,npgnum',_selectedpage,xojst') = changePageInXournalState npgnum xojst
          xstate' <- liftIO $ updatePageAll xojst' xstate 
          ncvsInfo <- liftIO $ setPage xstate' (PageNum npgnum') cid
          xstatefinal <- return . modifyCurrentCanvasInfo (const ncvsInfo) $ xstate'
          when b (commit xstatefinal)
          return xstatefinal 
        
        fcont xstate cvsInfo = do 
          let xojst = get xournalstate $ xstate  
              npgnum = modifyfn (get currentPageNum cvsInfo)
              cid = get canvasId cvsInfo
              (b,npgnum',_selectedpage,xojst') = changePageInXournalState npgnum xojst
          xstate' <- liftIO $ updatePageAll xojst' xstate 
          ncvsInfo <- liftIO $ setPage xstate' (PageNum npgnum') cid
          xstatefinal <- return . modifyCurrentCanvasInfo (const ncvsInfo) $ xstate'
          when b (commit xstatefinal)
          return xstatefinal 


-- | 
        
changePageInXournalState :: Int -> XournalState -> (Bool,Int,Page EditMode,XournalState)
changePageInXournalState npgnum xojstate =
    let exoj = xojstateEither xojstate 
        pgs = either (get g_pages) (get g_selectAll) exoj
        totnumpages = M.size pgs
        lpage = maybeError "changePage" (M.lookup (totnumpages-1) pgs)
        (isChanged,npgnum',npage',exoj') 
          | npgnum >= totnumpages = 
            let npage = newSinglePageFromOld lpage
                npages = M.insert totnumpages npage pgs 
            in (True,totnumpages,npage,
                either (Left . set g_pages npages) (Right. set g_selectAll npages) exoj )
          | otherwise = let npg = if npgnum < 0 then 0 else npgnum
                            pg = maybeError "changePage" (M.lookup npg pgs)
                        in (False,npg,pg,exoj) 
    in (isChanged,npgnum',npage',either ViewAppendState SelectState exoj')

-- | 

canvasZoomUpdateGenRenderCvsId :: MainCoroutine () 
                                  -> CanvasId 
                                  -> Maybe ZoomMode 
                                  -> MainCoroutine ()
canvasZoomUpdateGenRenderCvsId renderfunc cid mzmode 
  = updateXState zoomUpdateAction 
    >> adjustScrollbarWithGeometryCvsId cid
    >> renderfunc
  where zoomUpdateAction xst =  
          selectBoxAction (fsingle xst) (fcont xst) . getCanvasInfo cid $ xst 
        fsingle xstate cinfo = do   
          geometry <- liftIO $ getCvsGeomFrmCvsInfo cinfo 
          page <- getCurrentPageCvsId cid
          let zmode = maybe (get (zoomMode.viewInfo) cinfo) id mzmode          
              pdim = PageDimension $ get g_dimension page
              xy = either (const (0,0)) (unPageCoord.snd) 
                     (getCvsOriginInPage geometry)
              cdim = canvasDim geometry 
              narr = makeSingleArrangement zmode pdim cdim xy  
              ncinfobox = CanvasInfoBox
                          . set (pageArrangement.viewInfo) narr
                          . set (zoomMode.viewInfo) zmode $ cinfo
          return . modifyCanvasInfo cid (const ncinfobox) $ xstate
        fcont xstate cinfo = do   
          geometry <- liftIO $ getCvsGeomFrmCvsInfo cinfo 
          let zmode = maybe (get (zoomMode.viewInfo) cinfo) id mzmode          
              cpn = PageNum $ get currentPageNum cinfo 
              cdim = canvasDim geometry 
              xoj = getXournal xstate 
              origcoord = either (const (cpn,PageCoord (0,0))) id 
                            (getCvsOriginInPage geometry)
              narr = makeContinuousSingleArrangement zmode cdim xoj origcoord
              ncinfobox = CanvasInfoBox
                          . set (pageArrangement.viewInfo) narr
                          . set (zoomMode.viewInfo) zmode $ cinfo
          return . modifyCanvasInfo cid (const ncinfobox) $ xstate


-- | 

canvasZoomUpdateCvsId :: CanvasId 
                         -> Maybe ZoomMode 
                         -> MainCoroutine ()
canvasZoomUpdateCvsId = canvasZoomUpdateGenRenderCvsId invalidateAll
  
-- | 

canvasZoomUpdateBufAll :: MainCoroutine () 
canvasZoomUpdateBufAll = do 
    klst <- liftM (M.keys . getCanvasInfoMap) getSt
    mapM_ updatefunc klst 
  where 
    updatefunc cid 
      = canvasZoomUpdateGenRenderCvsId  (invalidateWithBuf cid) cid Nothing
-- |
          
canvasZoomUpdateAll :: MainCoroutine () 
canvasZoomUpdateAll = do 
  klst <- liftM (M.keys . getCanvasInfoMap) getSt
  mapM_ (flip canvasZoomUpdateCvsId Nothing) klst 


-- | 

canvasZoomUpdate :: Maybe ZoomMode -> MainCoroutine () 
canvasZoomUpdate mzmode = do  
  cid <- (liftM (getCurrentCanvasId) getSt)
  canvasZoomUpdateCvsId cid mzmode

-- |

pageZoomChange :: ZoomMode -> MainCoroutine () 
pageZoomChange = canvasZoomUpdate . Just 

-- | 

pageZoomChangeRel :: ZoomModeRel -> MainCoroutine () 
pageZoomChangeRel rzmode = do 
    boxAction fsingle . get currentCanvasInfo =<< getSt 
  where 
    fsingle :: (ViewMode a) => CanvasInfo a -> MainCoroutine ()
    fsingle cinfo = do 
      let czmode = get (zoomMode.viewInfo) cinfo 
          cpn = PageNum (get currentPageNum cinfo)
          arr = get (pageArrangement.viewInfo) cinfo 
          canvas = get drawArea cinfo 
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
    npgBfrAct xst = boxAction (fsimple xst) . get currentCanvasInfo $ xst
    fsimple :: (ViewMode a) => HXournalState -> CanvasInfo a 
               -> MainCoroutine HXournalState
    fsimple xstate cinfo = do 
      case get xournalstate xstate of 
        ViewAppendState xoj -> do 
          xoj' <- liftIO $ addNewPageInXoj dir xoj (get currentPageNum cinfo)
          return =<< liftIO . updatePageAll (ViewAppendState xoj')
                     . set xournalstate  (ViewAppendState xoj') $ xstate 
        SelectState _ -> do 
          liftIO $ putStrLn " not implemented yet"
          return xstate
      
-- | delete current page of current canvas
          
deleteCurrentPage :: MainCoroutine ()           
deleteCurrentPage = do 
    updateXState delpgact >> commit_ >> canvasZoomUpdateAll >> invalidateAll
  where 
    delpgact xst = boxAction (fsimple xst) . get currentCanvasInfo $ xst
    fsimple :: (ViewMode a) => HXournalState -> CanvasInfo a
               -> MainCoroutine HXournalState
    fsimple xstate cinfo = do 
      let cpn = PageNum (get currentPageNum cinfo)      
      case get xournalstate xstate of 
        ViewAppendState xoj -> do 
          xoj' <- liftIO $ deletePageInXoj xoj 
                             (PageNum (get currentPageNum cinfo))
          return =<< liftIO . updatePageAll (ViewAppendState xoj')
                     . set xournalstate  (ViewAppendState xoj') $ xstate 
        SelectState _ -> do 
          liftIO $ putStrLn " not implemented yet"
          return xstate
      
-- | delete designated page
          
deletePageInXoj :: Xournal EditMode -> PageNum -> IO (Xournal EditMode)
deletePageInXoj xoj (PageNum pgn) = do 
  putStrLn "deletePageInxoj is called"
  let pagelst = M.elems . get g_pages $ xoj 
      (pagesbefore,cpage:pagesafter) = splitAt pgn pagelst
      npage = newSinglePageFromOld cpage
      npagelst = pagesbefore ++ pagesafter
      nxoj = set g_pages (M.fromList . zip [0..] $ npagelst) xoj 
  return nxoj



