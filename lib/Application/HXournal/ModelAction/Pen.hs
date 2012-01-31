{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.ModelAction.Pen 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.ModelAction.Pen where


import Application.HXournal.ModelAction.Page
import Application.HXournal.ModelAction.Layer
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.Enum
import Application.HXournal.Type.PageArrangement
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
import Graphics.Xournal.Render.BBoxMapPDF
import Application.HXournal.View.Coordinate

addPDraw :: PenInfo -> TXournalBBoxMapPDFBuf -> PageNum -> Seq (Double,Double) 
            -> IO (TXournalBBoxMapPDFBuf,BBox)
addPDraw pinfo xoj (PageNum pgnum) pdraw = do 
  let ptype = get penType pinfo
      pcolor = get (penColor.currentTool) pinfo
      pcolname = fromJust (M.lookup pcolor penColorNameMap)
      pwidth = get (penWidth.currentTool) pinfo
      (mcurrlayer,currpage) = getCurrentLayerOrSet (getPageFromGXournalMap pgnum xoj)
      currlayer = maybe (error "something wrong in addPDraw") id mcurrlayer 
      newstroke = Stroke { stroke_tool = case ptype of 
                                           PenWork -> "pen" 
                                           HighlighterWork -> "highlighter"
                                           _ -> error "error in addPDraw"
                         , stroke_color = pcolname 
                         , stroke_width = pwidth
                         , stroke_data = map (uncurry (:!:)) . toList $ pdraw
                         } 
      newstrokebbox = mkStrokeBBoxFromStroke newstroke
      bbox = strokebbox_bbox newstrokebbox
  newlayerbbox <- updateLayerBuf (Just bbox)
                   . set g_bstrokes (get g_bstrokes currlayer ++ [newstrokebbox]) 
                   $ currlayer
  let newpagebbox = adjustCurrentLayer newlayerbbox currpage 
      newxojbbox = set g_pages (IM.adjust (const newpagebbox) pgnum (get g_pages xoj) ) xoj 
  return (newxojbbox,bbox)












