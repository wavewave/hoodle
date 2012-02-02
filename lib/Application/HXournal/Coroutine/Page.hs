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
import Application.HXournal.View.Draw
import Application.HXournal.View.Coordinate
import Application.HXournal.Accessor
import Application.HXournal.Coroutine.Draw
import Application.HXournal.Coroutine.Commit
import Application.HXournal.Coroutine.Scroll
-- import Application.HXournal.ModelAction.Adjustment
import Application.HXournal.ModelAction.Page
import Application.HXournal.Type.Alias
import Graphics.Xournal.Render.BBoxMapPDF
import Data.Xournal.Generic
import Graphics.UI.Gtk hiding (get,set)
import Control.Monad.Trans
import Control.Category
import Data.Label
import Prelude hiding ((.), id)
import Data.Xournal.Simple (Dimension(..))
import Data.Xournal.BBox
import qualified Data.IntMap as M

changePage :: (Int -> Int) -> MainCoroutine () 
changePage modifyfn = updateXState changePageAction 
                      >> (liftIO $ putStrLn "here")
                      >> adjustScrollbarWithGeometryCurrent
                      >> (liftIO $ putStrLn "here2")
                      >> invalidateCurrent
  where changePageAction xst = selectBoxAction (fsingle xst) (fcont xst) 
                               . get currentCanvasInfo $ xst
        fsingle xstate cvsInfo = do 
          let xojst = get xournalstate $ xstate  
              npgnum = modifyfn (get currentPageNum cvsInfo)
              (b,npgnum',selectedpage,xojst') = changePageInXournalState npgnum xojst
              xstate' = set xournalstate xojst' xstate
              Dim w h = get g_dimension selectedpage
          when b (commit xstate')
          ncvsInfo <- liftIO $ setPage xstate' (PageNum npgnum') (CanvasInfoBox cvsInfo)
          xstatefinal <- liftIO (updatePageAll xojst'
                                 . modifyCurrentCanvasInfo (const ncvsInfo)
                                 $ xstate')
          return xstatefinal 
            
        fcont xstate cvsInfo = do 
          let xojst = get xournalstate $ xstate  
              npgnum = modifyfn (get currentPageNum cvsInfo)
              (b,npgnum',selectedpage,xojst') = changePageInXournalState npgnum xojst
              xstate' = set xournalstate xojst' xstate
              Dim w h = get g_dimension selectedpage
          when b $ do {commit xstate' }
          ncvsInfo <- liftIO $ setPage xstate' (PageNum npgnum') (CanvasInfoBox cvsInfo)
          
          xstatefinal <- liftIO (updatePageAll xojst'
                                 . modifyCurrentCanvasInfo (const ncvsInfo)
                                 $ xstate')
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
{-
changePageInXournalState npgnum (SelectState txoj) = 
    let pgs = get g_selectAll txoj 
        totalnumofpages = M.size pgs
        lpage = maybeError "changePage" . M.lookup (totalnumofpages-1) $ pgs 
        (isChanged,npgnum',npage',txoj') 
          | npgnum  >= totalnumofpages = let npage = newSinglePageFromOld lpage
                                             npages = M.insert totalnumofpages npage pgs
                                             ntxoj = set g_selectAll npages txoj 
                                         in (True,totalnumofpages,npage,ntxoj) 
          | otherwise = let npg = if npgnum < 0 then 0 else npgnum  
                            pg = maybeError "changePage" (M.lookup npg pgs)
                        in (False,npg,pg,txoj)
    in (isChanged,npgnum',npage',SelectState txoj') -}


-- | 
    
canvasZoomUpdate :: Maybe ZoomMode -> MainCoroutine () 
canvasZoomUpdate mzmode = updateXState zoomUpdateAction 
                          >> adjustScrollbarWithGeometryCurrent
                          >> invalidateAll
  where zoomUpdateAction xst =  
          selectBoxAction (fsingle xst) (fcont xst) . get currentCanvasInfo $ xst 
          
        fsingle xstate cinfo = do   
          geometry <- liftIO $ getCvsGeomFrmCvsInfo cinfo 
          let zmode = maybe (get (zoomMode.viewInfo) cinfo) id mzmode          
              page = getPage cinfo 
              pdim = PageDimension $ get g_dimension page
              cdim = canvasDim geometry 
              narr = makeSingleArrangement zmode pdim cdim (0,0)
              ncinfobox = CanvasInfoBox
                          . set (pageArrangement.viewInfo) narr
                          . set (zoomMode.viewInfo) zmode $ cinfo
          return . modifyCurrentCanvasInfo (const ncinfobox) $ xstate
          
        fcont xstate cinfo = do   
          geometry <- liftIO $ getCvsGeomFrmCvsInfo cinfo 
          let zmode = maybe (get (zoomMode.viewInfo) cinfo) id mzmode          
              cpn = PageNum $ get currentPageNum cinfo 
              page = getPage cinfo 
              pdim = PageDimension $ get g_dimension page
              cdim = canvasDim geometry 
              xoj = getXournal xstate 
              narr = makeContinuousSingleArrangement zmode cdim xoj (cpn,PageCoord (0,0))
              ncinfobox = CanvasInfoBox
                          . set (pageArrangement.viewInfo) narr
                          . set (zoomMode.viewInfo) zmode $ cinfo
          return . modifyCurrentCanvasInfo (const ncinfobox) $ xstate

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


