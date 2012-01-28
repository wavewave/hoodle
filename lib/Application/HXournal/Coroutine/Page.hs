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

import Control.Applicative 
import Control.Monad
import Control.Compose
import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.PageArrangement
import Application.HXournal.Type.XournalState
import Application.HXournal.Util
import Application.HXournal.Draw
import Application.HXournal.Accessor
import Application.HXournal.Coroutine.Draw
import Application.HXournal.Coroutine.Commit
import Application.HXournal.ModelAction.Adjustment

import Graphics.Xournal.Render.BBoxMapPDF
import Data.Xournal.Generic
import Data.Xournal.Select 

import Graphics.UI.Gtk hiding (get,set)
import Application.HXournal.ModelAction.Page

import Control.Monad.Trans
import Control.Category
import Data.Label
import Prelude hiding ((.), id)
import Data.Xournal.Simple
import Data.Xournal.BBox
import qualified Data.IntMap as M

-- import Control.Arrow
-- import Control.Compose 

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
changePageInXournalState npgnum (ViewAppendState xoj) = 
    let pgs = get g_pages xoj 
        totalnumofpages = M.size pgs
        lpage = maybeError "changePage" (M.lookup (totalnumofpages-1) pgs)
        (isChanged,npgnum',npage',xoj') 
          | npgnum >= totalnumofpages = let npage = newSinglePageFromOld lpage
                                            npages = M.insert totalnumofpages npage pgs 
                                        in (True,totalnumofpages,npage,set g_pages npages xoj)
          | otherwise = let npg = if npgnum < 0 then 0 else npgnum
                            pg = maybeError "changePage" (M.lookup npg pgs)
                        in (False,npg,pg,xoj) 
    in (isChanged,npgnum',npage',ViewAppendState xoj')
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
    in (isChanged,npgnum',npage',SelectState txoj')


-- | 
    
canvasZoomUpdate :: Maybe ZoomMode -> MainCoroutine () 
canvasZoomUpdate mzmode = updateXState zoomUpdateAction 
  where zoomUpdateAction xst = 
          selectBoxAction (fsimple xst) (error "canvasZoomUpdate") . get currentCanvasInfo $ xst 
        fsimple xstate cvsInfo = do   
          let zmode = maybe (get (zoomMode.viewInfo) cvsInfo) id mzmode
          let canvas = get drawArea cvsInfo
          let page = getPage cvsInfo 
          let Dim w h = gdimension page
          cpg <- liftIO (getCanvasPageGeometry canvas page (0,0))        
          let (w',h') = canvas_size cpg 
          let (hadj,vadj) = get adjustments cvsInfo 
              s = 1.0 / getRatioFromPageToCanvas cpg zmode
          liftIO $ setAdjustments (hadj,vadj) (w,h) (0,0) (0,0) (w'*s,h'*s)
          let newvbbox = ViewPortBBox (BBox (0,0) (w'*s,h'*s))
          let ncvsInfo = CanvasInfoBox . set (zoomMode.viewInfo) zmode
                         . set (viewPortBBox.pageArrangement.viewInfo) newvbbox $ cvsInfo 
          return . modifyCurrentCanvasInfo (const ncvsInfo) $ xstate



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


{- (f &&& id) >>> g  
          where f = arr (selectBoxAction fsimple (error "canvasZoomUpdate") . get currentCanvasInfo )
                g = arr (uncurry modifyCurrentCanvasInfo) -}


{-         
              xstate' = modifyCurrentCanvasInfo setnewview xstate
          putSt xstate' 
          invalidate cid       -}




{-

                  else if modifyfn oldpage < 0 
                         then return (xstate,txoj,pgs,totalnumofpages,0)
                         else return (xstate,txoj,pgs,totalnumofpages,modifyfn oldpage)



              let xstate'' = updatePageAll (SelectState txoj')
                             . modifyCurrentCanvasInfo (setPage (SelectState txoj') newpage) 
                             $ xstate'
              return xstate''



    
                  else if modifyfn oldpage < 0 
                         then return (xstate,xoj,pgs,totalnumofpages,0)
                         else return (xstate,xoj,pgs,totalnumofpages,modifyfn oldpage)
              let Dim w h = get g_dimension lpage
                 (hadj,vadj) = get adjustments currCvsInfo
              liftIO $ do 
                adjustmentSetUpper hadj w 
                adjustmentSetUpper vadj h 
                adjustmentSetValue hadj 0
                adjustmentSetValue vadj 0
              return . updatePageAll (ViewAppendState xoj')
                     . modifyCurrentCanvasInfo (setPage (ViewAppendState xoj') newpage)  
                     $ xstate'



----
 
            ViewAppendState xoj -> do 

              (xstate',xoj',_pages',_totalnumofpages',newpage) <-
                        xstate' = set xournalstate (ViewAppendState newxoj) xstate
                    commit xstate'
                    return (xstate',newxoj,npages,totalnumofpages+1,totalnumofpages)




            SelectState txoj -> do 

              (xstate',txoj',_pages',_totalnumofpages',newpage) <-

                  then do
                    nlyr <- liftIO emptyTLayerBBoxBufLyBuf  
                    let npage = set g_layers (Select . O . Just . singletonSZ $ nlyr) lpage 
                        npages = IM.insert totalnumofpages npage pgs 
                        newtxoj = txoj { gselectAll = npages } 
                        xstate' = set xournalstate (SelectState newtxoj) xstate
                    commit xstate'
                    return (xstate',newtxoj,npages,totalnumofpages+1,totalnumofpages)

-}
--         putSt xstate'' 
--         invalidate currCvsId 

--         putSt xstate'' 
--         invalidate currCvsId 

         -- currCvsInfo' = setPage (ViewAppendState xoj') newpage currCvsInfo
        -- currCvsInfo' = setPage (SelectState txoj') newpage currCvsInfo 
    {- xstate <- getSt 
    let currCvsId = get currentCanvas xstate
        currCvsInfo = getCanvasInfo currCvsId xstate  -}

