{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Text.Hoodle.Migrate.V0_1_1_to_V0_2_2
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- attoparsec implementation of hoodle parser
-- 
-----------------------------------------------------------------------------

module Text.Hoodle.Migrate.V0_1_1_to_V0_2_2 where

import           Control.Applicative 
import           Control.Lens
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Data.Attoparsec
import qualified Data.ByteString.Char8 as B
--
import qualified Data.Hoodle.Simple.V0_1_1 as OH
import qualified Data.Hoodle.Simple.V0_2_2 as NH
-- 
import qualified Text.Hoodle.Parse.Attoparsec.V0_1_1 as OP 
import qualified Text.Hoodle.Parse.Attoparsec.V0_2_2 as NP

page2Page :: OH.Page -> NH.Page
page2Page OH.Page {..} = NH.Page { NH.page_dim = dim2Dim page_dim
                                 , NH.page_bkg = bkg2Bkg page_bkg
                                 , NH.page_layers = fmap layer2Layer page_layers } 

dim2Dim :: OH.Dimension -> NH.Dimension 
dim2Dim (OH.Dim w h) = NH.Dim w h 


bkg2Bkg :: OH.Background -> NH.Background 
bkg2Bkg OH.Background {..} = NH.Background { NH.bkg_type = bkg_type 
                                           , NH.bkg_color = bkg_color
                                           , NH.bkg_style = bkg_style }
bkg2Bkg OH.BackgroundPdf {..} = NH.BackgroundPdf { NH.bkg_type = bkg_type
                                                 , NH.bkg_domain = bkg_domain 
                                                 , NH.bkg_filename = bkg_filename
                                              ,   NH.bkg_pageno = bkg_pageno } 


item2Item :: OH.Item -> NH.Item 
item2Item (OH.ItemStroke strk) = NH.ItemStroke (stroke2Stroke strk)
item2Item (OH.ItemImage image) = NH.ItemImage (image2Image image)
item2Item (OH.ItemSVG svg) = NH.ItemSVG (svg2SVG svg)

stroke2Stroke :: OH.Stroke -> NH.Stroke
stroke2Stroke OH.Stroke {..} = NH.Stroke { NH.stroke_tool = stroke_tool 
                                         , NH.stroke_color = stroke_color
                                         , NH.stroke_width = stroke_width
                                         , NH.stroke_data = stroke_data } 
stroke2Stroke OH.VWStroke {..} = NH.VWStroke { NH.stroke_tool = stroke_tool 
                                             , NH.stroke_color = stroke_color
                                             , NH.stroke_vwdata = stroke_vwdata } 

image2Image :: OH.Image -> NH.Image
image2Image OH.Image {..} = NH.Image { NH.img_src = img_src 
                                     , NH.img_pos = img_pos
                                     , NH.img_dim = dim2Dim img_dim } 

svg2SVG :: OH.SVG -> NH.SVG
svg2SVG OH.SVG {..} = NH.SVG { NH.svg_text = svg_text
                             , NH.svg_command = svg_command 
                             , NH.svg_render = svg_render 
                             , NH.svg_pos = svg_pos 
                             , NH.svg_dim = dim2Dim svg_dim } 

{- 
link2Link :: OH.Link -> NH.Link
link2Link OH.Link {..} = NH.Link { NH.link_id = link_id 
                                 , NH.link_type = link_type
                                 , NH.link_location = link_location
                                 , NH.link_text = link_text
                                 , NH.link_command = link_command
                                 , NH.link_render = link_render
                                 , NH.link_pos = link_pos
                                 , NH.link_dim = dim2Dim link_dim }
-}                                                 
                                                     
layer2Layer :: OH.Layer -> NH.Layer
layer2Layer OH.Layer {..} = NH.Layer { NH.layer_items = fmap item2Item layer_items } 


migrate :: B.ByteString -> IO (Either String NH.Hoodle)
migrate bstr = do 
  runEitherT $ do 
    v <- hoistEither (parseOnly NP.checkHoodleVersion bstr)
    if v <= "0.1.1" 
      then do  
        oh <- hoistEither (parseOnly OP.hoodle bstr)
        let ttl = view OH.title oh 
            pgs = (fmap page2Page . view OH.pages) oh 
        set NH.title ttl . set NH.pages pgs <$> lift NH.emptyHoodle 
      else 
        hoistEither (parseOnly NP.hoodle bstr)

            -- pdf = view OH.embeddedPdf oh 
{- . set NH.embeddedPdf pdf -}


