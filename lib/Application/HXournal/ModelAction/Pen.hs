{-# LANGUAGE OverloadedStrings #-}

module Application.HXournal.ModelAction.Pen where

import Application.HXournal.ModelAction.Page
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Enum
import Data.Foldable
import Data.Maybe
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Control.Category
import Data.Sequence hiding (take, drop)
import Data.Label
import Prelude hiding ((.), id)
import Data.Strict.Tuple hiding (uncurry)
import Data.Xournal.Simple
import Data.Xournal.Generic
import Data.Xournal.BBox
import Graphics.Xournal.Render.Generic 
import Graphics.Xournal.Render.BBoxMapPDF
import Graphics.Xournal.Render.Simple
import Data.Xournal.Map


addPDraw :: PenInfo -> TXournalBBoxMapPDF -> Int -> Seq (Double,Double) 
            -> (TXournalBBoxMapPDF,BBox)
addPDraw pinfo xoj pgnum pdraw = 
  let pcolor = get penColor pinfo
      pcolname = fromJust (M.lookup pcolor penColorNameMap)
      pwidth = get penWidth pinfo
      currpage = getPageFromXojBBoxMapPDF  pgnum xoj
      currlayer = case IM.lookup 0 (glayers currpage) of
                    Nothing -> error "something wrong in addPDraw"
                    Just l -> l
      newstroke = Stroke { stroke_tool = "pen" 
                         , stroke_color = pcolname 
                         , stroke_width = pwidth
                         , stroke_data = map (uncurry (:!:)) . toList $ pdraw
                         } 
      newstrokebbox = mkStrokeBBoxFromStroke newstroke
      newlayerbbox =  set g_strokes (get g_strokes currlayer ++ [newstrokebbox]) currlayer
      newpagebbox = set g_layers (IM.adjust (const newlayerbbox) 0 (get g_layers currpage)) currpage 
      newxojbbox = set g_pages (IM.adjust (const newpagebbox) pgnum (get g_pages xoj) ) xoj 
      
  in  (newxojbbox,strokebbox_bbox newstrokebbox)


