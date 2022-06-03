{-# LANGUAGE TypeFamilies #-}

module Data.Xournal.Buffer where

import Data.IntMap (IntMap)
import Data.Xournal.BBox (StrokeBBox)
import Data.Xournal.Generic
  ( GLayerBuf,
    GPage,
    GXournal,
  )
import Data.Xournal.Select (ZipperSelect)

type TLayerBBoxBuf buf = GLayerBuf buf [] StrokeBBox

type TPageBBoxMapBkgBuf bkg buf = GPage bkg ZipperSelect (TLayerBBoxBuf buf)

type TXournalBBoxMapBkgBuf bkg buf =
  GXournal IntMap (TPageBBoxMapBkgBuf bkg buf)
