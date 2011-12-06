module Application.HXournal.Iteratee.Page where

import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Draw
import Application.HXournal.Accessor

import Application.HXournal.Iteratee.Draw

import Graphics.Xournal.Type 
import Graphics.Xournal.Type.Map
import Graphics.Xournal.Render.BBox


import Graphics.UI.Gtk hiding (get,set)

import Application.HXournal.ModelAction.Page

import Control.Applicative 
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import qualified Control.Monad.State as St 

import Control.Monad.Trans

import Control.Category
import Data.Label
import Prelude hiding ((.), id)

import Text.Xournal.Type
import qualified Data.IntMap as IM

changePage :: (Int -> Int) -> Iteratee MyEvent XournalStateIO () 
changePage modifyfn = do 
    xstate <- lift St.get 
    let currCvsId = get currentCanvas xstate
        cinfoMap = get canvasInfoMap xstate
        currCvsInfo = getCanvasInfo currCvsId xstate   
    let xojst = get xournalstate $ xstate 
    case xojst of 
      ViewAppendState xoj -> do 
        let pages = xbm_pages xoj 
            totalnumofpages = IM.size pages
            oldpage = get currentPageNum currCvsInfo
            lpage = case IM.lookup (totalnumofpages-1) pages of
                      Nothing -> error "error in changePage"
                      Just p -> p            
        (xstate',xoj',pages',totalnumofpages',newpage) <-
          if (modifyfn oldpage >= totalnumofpages) 
          then do 
            let npage = mkPageBBoxMapFromPageBBox 
                        . mkPageBBoxFromPage
                        . newPageFromOld 
                        . pageFromPageBBoxMap $ lpage
                npages = IM.insert totalnumofpages npage pages 
                newxoj = xoj { xbm_pages = npages } 
                xstate' = set xournalstate (ViewAppendState newxoj) xstate
            putSt xstate'
            return (xstate',newxoj,npages,totalnumofpages+1,totalnumofpages)
          else if modifyfn oldpage < 0 
                 then return (xstate,xoj,pages,totalnumofpages,0)
                 else return (xstate,xoj,pages,totalnumofpages,modifyfn oldpage)
        let Dim w h = pageDim lpage
            (hadj,vadj) = get adjustments currCvsInfo
        liftIO $ do 
          adjustmentSetUpper hadj w 
          adjustmentSetUpper vadj h 
          adjustmentSetValue hadj 0
          adjustmentSetValue vadj 0
        let currCvsInfo' = setPage (ViewAppendState xoj') newpage currCvsInfo 
            xstate'' = updatePageAll (ViewAppendState xoj')
                       . updateCanvasInfo currCvsInfo' 
                       $ xstate'
        lift . St.put $ xstate'' 
        invalidate currCvsId 
      SelectState _ -> do 
        error "not implemented yet : changePage" 
      

            
