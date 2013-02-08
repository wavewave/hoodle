{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Text.Hoodle.Translate.FromXournal 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Text.Hoodle.Translate.FromXournal 
( mkHoodleFromXournal      
) where

import qualified Data.Xournal.Simple as X 
import qualified Data.Hoodle.Simple as H

-- | 
mkHoodleFromXournal :: X.Xournal -> H.Hoodle 
mkHoodleFromXournal X.Xournal {..} = 
    H.Hoodle xoj_title Nothing (map x2h4Page xoj_pages) 
    
-- |     
x2h4Page :: X.Page -> H.Page
x2h4Page X.Page {..} = H.Page 
                         (x2h4dim page_dim) 
                         (x2h4bkg page_bkg) 
                         (map x2h4layer page_layers)
                         
-- |                          
x2h4dim :: X.Dimension -> H.Dimension
x2h4dim X.Dim {..} = H.Dim dim_width dim_height 

-- | 
x2h4bkg :: X.Background -> H.Background 
x2h4bkg X.Background {..} = H.Background bkg_type bkg_color bkg_style
x2h4bkg X.BackgroundPdf {..} = 
    H.BackgroundPdf bkg_type bkg_domain bkg_filename bkg_pageno


-- | 
x2h4layer :: X.Layer -> H.Layer 
x2h4layer X.Layer {..} = H.Layer (map x2h4stroke layer_strokes)

-- | 
x2h4stroke :: X.Stroke -> H.Item -- H.Stroke 
x2h4stroke X.Stroke {..} = 
    H.ItemStroke (H.Stroke stroke_tool stroke_color stroke_width stroke_data)
x2h4stroke X.VWStroke {..} = 
    H.ItemStroke (H.VWStroke stroke_tool stroke_color stroke_vwdata)
    
    

