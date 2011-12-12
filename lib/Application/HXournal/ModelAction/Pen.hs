{-# LANGUAGE OverloadedStrings #-}

module Application.HXournal.ModelAction.Pen where

import Application.HXournal.ModelAction.Page
import Graphics.Xournal.Type
import Graphics.Xournal.Type.Map
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
import Text.Xournal.Type

addPDraw :: PenInfo -> XournalBBoxMap -> Int -> Seq (Double,Double) 
            -> (XournalBBoxMap,BBox)
addPDraw pinfo xoj pgnum pdraw = 
  let pcolor = get penColor pinfo
      pcolname = fromJust (M.lookup pcolor penColorNameMap)
      pwidth = get penWidth pinfo
      currpage = getPageFromXojBBoxMap  pgnum xoj
      currlayer = case IM.lookup 0 (pbm_layers currpage) of
                    Nothing -> error "something wrong in addPDraw"
                    Just l -> l
      newstroke = Stroke { stroke_tool = "pen" 
                         , stroke_color = pcolname 
                         , stroke_width = pwidth
                         , stroke_data = map (uncurry (:!:)) . toList $ pdraw
                         } 
      newstrokebbox = mkStrokeBBoxFromStroke newstroke
      newlayerbbox =  currlayer {layerbbox_strokes = layerStrokes currlayer 
                                                    ++ [newstrokebbox] }
      newpagebbox = currpage { 
        pbm_layers = IM.adjust (const newlayerbbox) 0 (pbm_layers currpage) 
      }

      newxojbbox = xoj { 
        xbm_pages = IM.adjust (const newpagebbox) pgnum (xbm_pages xoj) 
      }
  in  (newxojbbox,strokebbox_bbox newstrokebbox)


