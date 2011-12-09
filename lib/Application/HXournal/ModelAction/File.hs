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

getFileContent :: DeviceList 
               -> (DrawingArea,Adjustment,Adjustment) 
               -> (DrawingArea,Adjustment,Adjustment)
               -> (MyEvent -> IO ()) 
               -> FilePath 
               -> IO HXournalState 
getFileContent devlst (canvas,hadj,vadj) (canvas2,hadj2,vadj2) callbackfunc fname = do 
  xojcontent <- P.read_xournal fname 
  let xojWbbox = mkXournalBBoxMapFromXournal xojcontent 
  let Dim width height = pageDim . (!! 0) .  xournalPages $ xojcontent
      startingxojstate = ViewAppendState xojWbbox
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
      startingcinfo1 = setPage startingxojstate 0 cinfo1
      startingcinfo2 = setPage startingxojstate 0 cinfo2
      cinfoMap = M.insert (get canvasId startingcinfo2) startingcinfo2
               $ M.insert (get canvasId startingcinfo1) startingcinfo1
               $ M.empty 
  let st = set xournalstate startingxojstate
         . set currFileName (Just fname)
         . set canvasInfoMap cinfoMap 
         . set currentCanvas (get canvasId startingcinfo1)
         . set deviceList devlst 
         . set callBack callbackfunc 
         $ emptyHXournalState
  return st 
