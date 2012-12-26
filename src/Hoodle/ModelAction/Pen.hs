{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.ModelAction.Pen 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.ModelAction.Pen where

import           Control.Category
import           Control.Lens
import           Data.Foldable
import qualified Data.IntMap as IM
import           Data.Maybe
import qualified Data.Map as M
import           Data.Sequence hiding (take, drop)
import           Data.Strict.Tuple hiding (uncurry)
-- from hoodle-platform 
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Type
-- from this package 
import           Hoodle.ModelAction.Layer
import           Hoodle.ModelAction.Page
import           Hoodle.Type.Canvas
import           Hoodle.Type.Enum
import           Hoodle.Type.PageArrangement
--
import Prelude hiding ((.), id)

-- | 
addPDraw :: PenInfo 
            -> RHoodle
            -> PageNum 
            -> Seq (Double,Double,Double) 
            -> IO (RHoodle,BBox) 
                       -- ^ new hoodle and bbox in page coordinate
addPDraw pinfo hdl (PageNum pgnum) pdraw = do 
    let ptype = view penType pinfo
        pcolor = view (currentTool.penColor) pinfo
        pcolname = convertPenColorToByteString pcolor 
        pwidth = view (currentTool.penWidth) pinfo
        pvwpen = view variableWidthPen pinfo
        (mcurrlayer,currpage) = getCurrentLayerOrSet (getPageFromGHoodleMap pgnum hdl)
        dim = view gdimension currpage
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
                                           
        newstrokebbox = mkStrokeBBox newstroke
        bbox = strkbbx_bbx newstrokebbox
    newlayerbbox <- updateLayerBuf dim (Just bbox)
                    . over gitems (++[RItemStroke newstrokebbox]) 
                    $ currlayer

    let newpagebbox = adjustCurrentLayer newlayerbbox currpage 
        newhdlbbox = set gpages (IM.adjust (const newpagebbox) pgnum (view gpages hdl) ) hdl 
    return (newhdlbbox,bbox)

