module Application.HXournal.Iteratee.Page where

import Application.HXournal.Type.Event
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Draw
import Application.HXournal.Accessor

import Application.HXournal.Iteratee.Draw

import Graphics.Xournal.Type 
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
import qualified Data.Map as M

changePage :: (Int -> Int) -> Iteratee MyEvent XournalStateIO () 
changePage modifyfn = do 
  xstate <- lift St.get 
  let currCvsId = get currentCanvas xstate
      cinfoMap = get canvasInfoMap xstate
      maybeCurrCvs = M.lookup currCvsId cinfoMap 
  case maybeCurrCvs of 
    Nothing -> return ()
    Just currCvsInfo -> do 
      let xoj = unView . get xournalstate $ xstate 
          pages = xournalPages xoj 
          totalnumofpages = length pages
          oldpage = get currentPageNum currCvsInfo
      (xstate',xoj',pages',totalnumofpages',newpage) <-
         if (modifyfn oldpage >= totalnumofpages) 
           then do let lpage = last pages
                       npage = mkPageBBoxFromPage 
                             . newPageFromOld 
                             . pageFromPageBBox $ lpage
                       npages = pages ++ [npage] 
                       newxoj = xoj { xojbbox_pages = npages } 
                       xstate' = set xournalstate (ViewAppendState newxoj) xstate
                   putSt xstate'
                   return (xstate',newxoj,npages,totalnumofpages+1,totalnumofpages)
           else if modifyfn oldpage < 0 
                  then return (xstate,xoj,pages,totalnumofpages,0)
                  else return (xstate,xoj,pages,totalnumofpages,modifyfn oldpage)
      let Dim w h = pageDim . (!! newpage) $ pages'
          (hadj,vadj) = get adjustments currCvsInfo
      liftIO $ do 
        adjustmentSetUpper hadj w 
        adjustmentSetUpper vadj h 
        adjustmentSetValue hadj 0
        adjustmentSetValue vadj 0
      let currCvsInfo' = setPage (ViewAppendState xoj') newpage currCvsInfo 
          cinfoMap' = M.adjust (\_ -> currCvsInfo') currCvsId cinfoMap  
          xstate'' = set canvasInfoMap cinfoMap' xstate'
      lift . St.put $ xstate'' 
      invalidate currCvsId 

            
            
{-        
  = set (viewPortOrigin.viewInfo) (0,0) 
   . set (pageDimension.viewInfo) (w,h) 
   . set currentPageNum newpage
   $ currCvsInfo -}
                        
