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
import Application.HXournal.Util
import Application.HXournal.View.Coordinate
import Application.HXournal.Accessor
import Application.HXournal.Coroutine.Draw
import Application.HXournal.Coroutine.Commit
import Application.HXournal.Coroutine.Scroll
-- import Application.HXournal.ModelAction.Adjustment
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
              -- Dim w h = get g_dimension selectedpage
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
              -- Dim w h = get g_dimension selectedpage
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
              cdim = canvasDim geometry 
              narr = makeSingleArrangement zmode pdim cdim (0,0)
              ncinfobox = CanvasInfoBox
                          . set (pageArrangement.viewInfo) narr
                          . set (zoomMode.viewInfo) zmode $ cinfo
          return . modifyCanvasInfo cid (const ncinfobox) $ xstate
        fcont xstate cinfo = do   
          geometry <- liftIO $ getCvsGeomFrmCvsInfo cinfo 
          -- page <- getCurrentPageCvsId cid          
          let zmode = maybe (get (zoomMode.viewInfo) cinfo) id mzmode          
              cpn = PageNum $ get currentPageNum cinfo 
              -- pdim = PageDimension $ get g_dimension page
              cdim = canvasDim geometry 
              xoj = getXournal xstate 
              narr = makeContinuousSingleArrangement zmode cdim xoj (cpn,PageCoord (0,0))
              ncinfobox = CanvasInfoBox
                          . set (pageArrangement.viewInfo) narr
                          . set (zoomMode.viewInfo) zmode $ cinfo
          return . modifyCanvasInfo cid (const ncinfobox) $ xstate


-- | 

canvasZoomUpdateCvsId :: CanvasId 
                         -> Maybe ZoomMode 
                         -> MainCoroutine ()
canvasZoomUpdateCvsId = canvasZoomUpdateGenRenderCvsId invalidateAll
  
{-  
  cid mzmode = updateXState zoomUpdateAction 
                                   >> adjustScrollbarWithGeometryCvsId cid
                                   >> invalidateAll
  where zoomUpdateAction xst =  
          selectBoxAction (fsingle xst) (fcont xst) . getCanvasInfo cid $ xst 
          
        fsingle xstate cinfo = do   
          geometry <- liftIO $ getCvsGeomFrmCvsInfo cinfo 
          page <- getCurrentPageCvsId cid
          let zmode = maybe (get (zoomMode.viewInfo) cinfo) id mzmode          
              pdim = PageDimension $ get g_dimension page
              cdim = canvasDim geometry 
              narr = makeSingleArrangement zmode pdim cdim (0,0)
              ncinfobox = CanvasInfoBox
                          . set (pageArrangement.viewInfo) narr
                          . set (zoomMode.viewInfo) zmode $ cinfo
          return . modifyCanvasInfo cid (const ncinfobox) $ xstate
          
        fcont xstate cinfo = do   
          geometry <- liftIO $ getCvsGeomFrmCvsInfo cinfo 
          page <- getCurrentPageCvsId cid          
          let zmode = maybe (get (zoomMode.viewInfo) cinfo) id mzmode          
              cpn = PageNum $ get currentPageNum cinfo 
              pdim = PageDimension $ get g_dimension page
              cdim = canvasDim geometry 
              xoj = getXournal xstate 
              narr = makeContinuousSingleArrangement zmode cdim xoj (cpn,PageCoord (0,0))
              ncinfobox = CanvasInfoBox
                          . set (pageArrangement.viewInfo) narr
                          . set (zoomMode.viewInfo) zmode $ cinfo
          return . modifyCanvasInfo cid (const ncinfobox) $ xstate
-}

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

{-

-- |

newPageBefore :: MainCoroutine () 
newPageBefore = do 
  liftIO $ putStrLn "newPageBefore called"
  xstate <- getSt
  let xojstate = get xournalstate xstate
  case xojstate of 
    ViewAppendState xoj -> do 
      liftIO $ putStrLn " In View " 
      let currCvsId = get currentCanvas xstate 
          mcurrCvsInfo = M.lookup currCvsId (get canvasInfoMap xstate)
      xoj' <- maybe (error $ "something wrong in newPageBefore")
                    (liftIO . newPageBeforeAction xoj)
                    $ (,) <$> pure currCvsId <*> mcurrCvsInfo  
      let xstate' = updatePageAll (ViewAppendState xoj')
                    . set xournalstate  (ViewAppendState xoj') 
                    $ xstate 
      commit xstate'
      invalidate currCvsId 
    SelectState txoj -> liftIO $ putStrLn " In Select State, this is not implemented yet."

-}


