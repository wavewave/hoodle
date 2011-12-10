module Application.HXournal.ModelAction.File where

import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Event 
import Application.HXournal.Device
import Application.HXournal.ModelAction.Page

import Graphics.Xournal.Type.Map

import Graphics.UI.Gtk hiding (get,set)
import Text.Xournal.Type
import qualified Text.Xournal.Parse as P

import qualified Data.IntMap as M

import Control.Category
import Data.Label
import Prelude hiding ((.),id)

{-                
DeviceList 
-> (DrawingArea,Adjustment,Adjustment) 
               -> (DrawingArea,Adjustment,Adjustment) 
               -> (MyEvent -> IO ()) 

-}
{-                      
      cinfo1 = set canvasId 1 
             . set drawArea canvas
             . set viewInfo (ViewInfo OnePage Original (0,0) (width,height))
             . set currentPageNum 0 
             . set horizAdjustment hadj 
             . set vertAdjustment vadj 
             $ emptyCanvasInfo
      cinfo2 = set canvasId 2 
             . set drawArea canvas2
             . set viewInfo (ViewInfo OnePage Original (0,0) (width,height))
             . set currentPageNum 0 
             . set horizAdjustment hadj2 
             . set vertAdjustment vadj2 
             $ emptyCanvasInfo

      -- startingcinfo1 = setPage startingxojstate 0 cinfo1
      -- startingcinfo2 = setPage startingxojstate 0 cinfo2
      --  cinfoMap = M.insert (get canvasId startingcinfo2) startingcinfo2
      --         $ M.insert (get canvasId startingcinfo1) startingcinfo1
      --          $ M.empty 

-}

-- | get file content from xournal file and update xournal state 

getFileContent :: FilePath 
               -> HXournalState 
               -> IO HXournalState 
getFileContent fname xstate = do 
  xojcontent <- P.read_xournal fname 
  let currcid = get currentCanvas xstate 
      cmap = get canvasInfoMap xstate 
  let xojWbbox = mkXournalBBoxMapFromXournal xojcontent 
  let Dim width height = pageDim . (!! 0) .  xournalPages $ xojcontent
      startingxojstate = ViewAppendState xojWbbox
      cids = M.keys cmap 
      update x cinfo = 
        let changefunc cinfo = 
              setPage startingxojstate 0 
              . set viewInfo (ViewInfo OnePage Original (0,0) (width,height))
              . set currentPageNum 0 
              $ cinfo 
        in  M.adjust changefunc x cmap  
      cmap' = foldr update cmap cids 
  let newxstate = set xournalstate startingxojstate
                  . set currFileName (Just fname)
                  . set canvasInfoMap cmap'
                  . set currentCanvas currcid 
                  $ xstate
  return newxstate 
