{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.Hoodle.ModelAction.Pen 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.Hoodle.ModelAction.Pen where


import Application.Hoodle.ModelAction.Page
import Application.Hoodle.ModelAction.Layer
import Application.Hoodle.Type.Canvas
import Application.Hoodle.Type.Enum
import Application.Hoodle.Type.PageArrangement
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

-- | 

addPDraw :: PenInfo 
            -> TXournalBBoxMapPDFBuf 
            -> PageNum 
            -> Seq (Double,Double,Double) 
            -> IO (TXournalBBoxMapPDFBuf,BBox) 
                       -- ^ new xournal and bbox in page coordinate
addPDraw pinfo xoj (PageNum pgnum) pdraw = do 
    let ptype = get penType pinfo
        pcolor = get (penColor.currentTool) pinfo
        pcolname = fromJust (M.lookup pcolor penColorNameMap)
        pwidth = get (penWidth.currentTool) pinfo
        pvwpen = get variableWidthPen pinfo
        (mcurrlayer,currpage) = getCurrentLayerOrSet (getPageFromGXournalMap pgnum xoj)
        currlayer = maybe (error "something wrong in addPDraw") id mcurrlayer 
        ptool = case ptype of 
                  PenWork -> "pen" 
                  HighlighterWork -> "highlighter"
                  _ -> error "error in addPDraw"
        newstroke = 
          case pvwpen of 
            False -> Stroke { stroke_tool = ptool 
                            , stroke_color = pcolname 
                            , stroke_width = pwidth
                            , stroke_data = map (\(x,y,_)->x:!:y) . toList $ pdraw
                          } 
            True -> VWStroke { stroke_tool = ptool
                             , stroke_color = pcolname                 
                             , stroke_vwdata = map (\(x,y,z)->(x,y,pwidth*z)) . toList $ pdraw
                             }
                                           
        newstrokebbox = mkStrokeBBoxFromStroke newstroke
        bbox = strokebbox_bbox newstrokebbox
    newlayerbbox <- updateLayerBuf (Just bbox)
                    . set g_bstrokes (get g_bstrokes currlayer ++ [newstrokebbox]) 
                    $ currlayer

    let newpagebbox = adjustCurrentLayer newlayerbbox currpage 
        newxojbbox = set g_pages (IM.adjust (const newpagebbox) pgnum (get g_pages xoj) ) xoj 
    return (newxojbbox,bbox)

