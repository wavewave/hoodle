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
import Application.HXournal.ModelAction.Adjustment
import Graphics.Xournal.Render.BBoxMapPDF
import Data.Xournal.Generic
import Graphics.UI.Gtk hiding (get,set)
import Application.HXournal.ModelAction.Page
import Control.Monad.Trans
import Control.Category
import Data.Label
import Prelude hiding ((.), id)
import Data.Xournal.Simple
import Data.Xournal.BBox
import qualified Data.IntMap as M

changePage :: (Int -> Int) -> MainCoroutine () 
changePage modifyfn = updateXState changePageAction >> invalidateCurrent
  where changePageAction xst = selectBoxAction (fsimple xst) (error "changePage") . get currentCanvasInfo $ xst
        fsimple xstate cvsInfo = do 
          let xojst = get xournalstate $ xstate  
              npgnum = modifyfn (get currentPageNum cvsInfo)
              (b,npgnum',selectedpage,xojst') = changePageInXournalState npgnum xojst
              xstate' = set xournalstate xojst' xstate
              Dim w h = get g_dimension selectedpage
              (hadj,vadj) = get adjustments cvsInfo
          when b (commit xstate')
          liftIO $ do adjustmentSetUpper hadj w 
                      adjustmentSetUpper vadj h 
                      adjustmentSetValue hadj 0
                      adjustmentSetValue vadj 0
          let ncvsInfo = CanvasInfoBox . setPage xojst' npgnum' $ cvsInfo
          return . updatePageAll xojst'
                 . modifyCurrentCanvasInfo (const ncvsInfo)
                 $ xstate'



        
changePageInXournalState :: Int -> XournalState -> (Bool,Int,TPageBBoxMapPDFBuf,XournalState)
changePageInXournalState npgnum xojstate = -- (ViewAppendState xoj) = 
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
canvasZoomUpdate mzmode = updateXState zoomUpdateAction >> invalidateAll
  where zoomUpdateAction xst = 
          selectBoxAction (fsimple xst) (error "canvasZoomUpdate") . get currentCanvasInfo $ xst 
        fsimple xstate cinfo = do   
          let zmode = maybe (get (zoomMode.viewInfo) cinfo) id mzmode
          let canvas = get drawArea cinfo
          let page = getPage cinfo
              pn  = PageNum . get currentPageNum $ cinfo 
          let pdim@(Dim w h) = get g_dimension page
              arr = get (pageArrangement.viewInfo) cinfo 
          geometry <- liftIO (makeCanvasGeometry (pn,page) arr canvas)
          let cdim@(CanvasDimension (Dim w' h')) = canvasDim geometry 
          let (sinvx,sinvy) = 
                getRatioPageCanvas zmode (PageDimension pdim) cdim
              (sx,sy) = (1.0/sinvx, 1.0/sinvy)
              newvbbox = ViewPortBBox (BBox (0,0) (w'*sx,h'*sy)) 
          let ncinfo = CanvasInfoBox . set (zoomMode.viewInfo) zmode
                         . set (viewPortBBox.pageArrangement.viewInfo) newvbbox $ cinfo
          let (hadj,vadj) = get adjustments cinfo 
          liftIO $ setAdjustments (hadj,vadj) (w,h) (0,0) (0,0) (w'*sx,h'*sy)
          return . modifyCurrentCanvasInfo (const ncinfo) $ xstate
          

{-          let (w',h') = canvas_size cpg 
          let (hadj,vadj) = get adjustments cvsInfo 
              s = 1.0 / getRatioFromPageToCanvas cpg zmode
          liftIO $ setAdjustments (hadj,vadj) (w,h) (0,0) (0,0) (w'*s,h'*s)
          let newvbbox = ViewPortBBox (BBox (0,0) (w'*s,h'*s))
          let ncvsInfo = CanvasInfoBox . set (zoomMode.viewInfo) zmode
                         . set (viewPortBBox.pageArrangement.viewInfo) newvbbox $ cvsInfo  -}


          -- cpg <- liftIO (getCanvasPageGeometry canvas page (0,0))        



pageZoomChange :: ZoomMode -> MainCoroutine () 
pageZoomChange = canvasZoomUpdate . Just 

{-

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


