module Application.HXournal.ModelAction.File where

import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Canvas
import Application.HXournal.ModelAction.Page
import qualified Text.Xournal.Parse as P
import qualified Data.IntMap as M
import Control.Category
import Data.Label
import Prelude hiding ((.),id)

import Data.Xournal.Map
import Data.Xournal.Simple
import Data.Xournal.Generic
import Graphics.Xournal.Render.Generic
import Graphics.Xournal.Render.BBoxMapPDF
import Graphics.Xournal.Render.PDFBackground

-- | get file content from xournal file and update xournal state 

getFileContent :: Maybe FilePath 
               -> HXournalState 
               -> IO HXournalState 
getFileContent (Just fname) xstate = do 
    xojcontent <- P.read_xournal fname 
    let currcid = get currentCanvas xstate 
        cmap = get canvasInfoMap xstate 
    xoj <- mkTXournalBBoxMapPDF xojcontent 
    let Dim width height = case M.lookup 0 (gpages xoj) of    
                             Nothing -> error "no first page in getFileContent" 
                             Just p -> gdimension p 
        startingxojstate = ViewAppendState xoj
        cids = M.keys cmap 
        update x _cinfo = 
          let changefunc c = 
                setPage startingxojstate 0 
                . set viewInfo (ViewInfo OnePage Original (0,0) (width,height))
                . set currentPageNum 0 
                $ c 
          in  M.adjust changefunc x cmap  
        cmap' = foldr update cmap cids   
    let newxstate = set xournalstate startingxojstate
                    . set currFileName (Just fname)
                    . set canvasInfoMap cmap'
                    . set currentCanvas currcid 
                    $ xstate
    return newxstate 
getFileContent Nothing xstate = do   
    newxoj <- mkTXournalBBoxMapPDF defaultXournal 
    let newxojstate = ViewAppendState newxoj 
        xstate' = set currFileName Nothing 
                  . set xournalstate newxojstate
                  $ xstate 
        cmap = get canvasInfoMap xstate'
    let Dim w h = page_dim . (!! 0) .  xoj_pages $ defaultXournal
        ciupdt = setPage newxojstate 0                       
                 . set viewInfo (ViewInfo OnePage Original (0,0) (w,h))
                 . set currentPageNum 0 
        cmap' = M.map ciupdt cmap
    return (set canvasInfoMap cmap' xstate')
