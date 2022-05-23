{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Hoodle.Migrate.V0_2_2_to_V0_3 where

import Control.Error.Util (hoistEither)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8 as B
--
import qualified Data.Hoodle.Simple.V0_2_2 as OH
import qualified Data.Hoodle.Simple.V0_3 as NH
import Lens.Micro
import Lens.Micro.Extras (view)
--
import qualified Text.Hoodle.Migrate.V0_1_1_to_V0_2_2
import qualified Text.Hoodle.Parse.Attoparsec.V0_2_2 as OP
import qualified Text.Hoodle.Parse.Attoparsec.V0_3 as NP

dim2Dim :: OH.Dimension -> NH.Dimension
dim2Dim (OH.Dim w h) = NH.Dim w h

bkg2Bkg :: OH.Background -> NH.Background
bkg2Bkg OH.Background {..} =
  NH.Background
    { NH.bkg_type = bkg_type,
      NH.bkg_color = bkg_color,
      NH.bkg_style = bkg_style
    }
bkg2Bkg OH.BackgroundPdf {..} =
  NH.BackgroundPdf
    { NH.bkg_type = bkg_type,
      NH.bkg_domain = bkg_domain,
      NH.bkg_filename = bkg_filename,
      NH.bkg_pageno = bkg_pageno
    }
bkg2Bkg OH.BackgroundEmbedPdf {..} =
  NH.BackgroundEmbedPdf
    { NH.bkg_type = bkg_type,
      NH.bkg_pageno = bkg_pageno
    }

item2Item :: OH.Item -> NH.Item
item2Item (OH.ItemStroke strk) = NH.ItemStroke (stroke2Stroke strk)
item2Item (OH.ItemImage image) = NH.ItemImage (image2Image image)
item2Item (OH.ItemSVG svg) = NH.ItemSVG (svg2SVG svg)
item2Item (OH.ItemLink lnk) = NH.ItemLink (link2Link lnk)

stroke2Stroke :: OH.Stroke -> NH.Stroke
stroke2Stroke OH.Stroke {..} =
  NH.Stroke
    { NH.stroke_tool = stroke_tool,
      NH.stroke_color = stroke_color,
      NH.stroke_width = stroke_width,
      NH.stroke_data = stroke_data
    }
stroke2Stroke OH.VWStroke {..} =
  NH.VWStroke
    { NH.stroke_tool = stroke_tool,
      NH.stroke_color = stroke_color,
      NH.stroke_vwdata = stroke_vwdata
    }

image2Image :: OH.Image -> NH.Image
image2Image OH.Image {..} =
  NH.Image
    { NH.img_src = img_src,
      NH.img_pos = img_pos,
      NH.img_dim = dim2Dim img_dim
    }

svg2SVG :: OH.SVG -> NH.SVG
svg2SVG OH.SVG {..} =
  NH.SVG
    { NH.svg_text = svg_text,
      NH.svg_command = svg_command,
      NH.svg_render = svg_render,
      NH.svg_pos = svg_pos,
      NH.svg_dim = dim2Dim svg_dim
    }

link2Link :: OH.Link -> NH.Link
link2Link OH.Link {..} =
  NH.Link
    { NH.link_id = link_id,
      NH.link_type = link_type,
      NH.link_location = link_location,
      NH.link_text = link_text,
      NH.link_command = link_command,
      NH.link_render = link_render,
      NH.link_pos = link_pos,
      NH.link_dim = dim2Dim link_dim
    }
link2Link OH.LinkDocID {..} =
  NH.LinkDocID
    { NH.link_id = link_id,
      NH.link_linkeddocid = link_linkeddocid,
      NH.link_location = link_location,
      NH.link_text = link_text,
      NH.link_command = link_command,
      NH.link_render = link_render,
      NH.link_pos = link_pos,
      NH.link_dim = dim2Dim link_dim
    }

layer2Layer :: OH.Layer -> NH.Layer
layer2Layer OH.Layer {..} = NH.Layer {NH.layer_items = fmap item2Item layer_items}

page2Page :: OH.Page -> NH.Page
page2Page OH.Page {..} =
  NH.Page
    { NH.page_dim = dim2Dim page_dim,
      NH.page_bkg = bkg2Bkg page_bkg,
      NH.page_layers = fmap layer2Layer page_layers
    }

rev2Rev :: OH.Revision -> NH.Revision
rev2Rev OH.Revision {..} =
  NH.Revision
    { NH._revmd5 = _revmd5,
      NH._revtxt = _revtxt
    }
rev2Rev OH.RevisionInk {..} =
  NH.RevisionInk
    { NH._revmd5 = _revmd5,
      NH._revink = map stroke2Stroke _revink
    }

hoodle2Hoodle :: OH.Hoodle -> IO NH.Hoodle
hoodle2Hoodle oh =
  set NH.hoodleID (view OH.hoodleID oh)
    . set NH.title (view OH.title oh)
    . set NH.revisions (map rev2Rev (view OH.revisions oh))
    . set NH.embeddedPdf (view OH.embeddedPdf oh)
    . set NH.pages ((fmap page2Page . view OH.pages) oh)
    <$> NH.emptyHoodle

migrate :: B.ByteString -> IO (Either String NH.Hoodle)
migrate bstr = do
  runExceptT $ do
    v <- hoistEither (parseOnly NP.checkHoodleVersion bstr)
    if
        | v <= "0.1.1" -> do
          oh <- ExceptT (Text.Hoodle.Migrate.V0_1_1_to_V0_2_2.migrate bstr)
          let ttl = view OH.title oh
              pgs = (fmap page2Page . view OH.pages) oh
          set NH.title ttl . set NH.pages pgs <$> lift NH.emptyHoodle
        | v <= "0.2.2" && v > "0.1.1" ->
          do
            oh <- hoistEither (parseOnly OP.hoodle bstr)
            lift (hoodle2Hoodle oh)
        | otherwise -> hoistEither (parseOnly NP.hoodle bstr)

-- pdf = view OH.embeddedPdf oh
{- . set NH.embeddedPdf pdf -}
