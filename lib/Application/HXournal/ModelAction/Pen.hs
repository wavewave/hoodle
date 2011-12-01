{-# LANGUAGE OverloadedStrings #-}

module Application.HXournal.ModelAction.Pen where

import Graphics.Xournal.Type
import Graphics.Xournal.Render.BBox
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Enum

import Data.Foldable
import Data.Maybe
import qualified Data.Map as M


import Control.Category
import Data.Sequence hiding (take, drop)
import Data.Label
import Prelude hiding ((.), id)

import Data.Strict.Tuple hiding (uncurry)
import Text.Xournal.Type

addPDraw :: PenInfo -> XournalBBox -> Int -> Seq (Double,Double) 
            -> (XournalBBox,BBox)
addPDraw pinfo xoj pgnum pdraw = 
  let pcolor = get penColor pinfo
      pcolname = fromJust (M.lookup pcolor penColorNameMap)
      pwidth = get penWidth pinfo
      pages = xournalPages xoj
      pagesbefore = take pgnum pages 
      pagesafter  = drop (pgnum+1) pages 
      currpage = pages !! pgnum
      currlayer = head (pageLayers currpage)
      otherlayers = tail (pageLayers currpage)
      newstroke = Stroke { stroke_tool = "pen" 
                         , stroke_color = pcolname 
                         , stroke_width = pwidth
                         , stroke_data = map (uncurry (:!:)) . toList $ pdraw
                         } 
      newstrokebbox = mkStrokeBBoxFromStroke newstroke
      newlayerbbox = currlayer {layerbbox_strokes = layerStrokes currlayer 
                                                    ++ [newstrokebbox] }
      newpagebbox = currpage {pagebbox_layers = newlayerbbox : otherlayers }
      newxojbbox = xoj { xojbbox_pages =  pagesbefore 
                                          ++ [newpagebbox] 
                                          ++ pagesafter }  

  in  (newxojbbox,strokebbox_bbox newstrokebbox)
