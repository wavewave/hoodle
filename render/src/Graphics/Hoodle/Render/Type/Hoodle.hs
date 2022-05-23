{-# LANGUAGE FlexibleInstances #-}

module Graphics.Hoodle.Render.Type.Hoodle where

import Control.Lens
import Data.Foldable (toList)
import Data.Hoodle.BBox
import Data.Hoodle.Generic
import Data.Hoodle.Simple
import Data.Hoodle.Zipper
import qualified Data.IntMap as IM
import Graphics.Hoodle.Render.Type.Background
import Graphics.Hoodle.Render.Type.Item
import Graphics.Hoodle.Render.Type.Renderer

----------------------------
-- normal state rendering --
----------------------------

newtype LyBuf = LyBuf {unLyBuf :: SurfaceID}

-- | normal rendering data structure for layer, R for rendering
--   buffer is Surface, container for item = list
--   and StrokeBBox as contained items
type RLayer = GLayer LyBuf [] RItem

-- | normal rendering data structure for page
--   background is RBackground and container for layer is IntMap
--   and layer is RLayer
type RPage = GPage RBackground ZipperSelect RLayer

-- | normal rendering data struture for hoodle
--   container for page is IntMap
--   page is RPage
type RHoodle = GHoodle IM.IntMap RPage

instance Show RHoodle where
  show _ = "RHoodle"

emptyRLayer :: SurfaceID -> RLayer
emptyRLayer sfcid = GLayer (LyBuf sfcid) []

-------
-- get simple hoodle data structure
-------

-- | project to simple Layer out of RLayer
rLayer2Layer :: RLayer -> Layer
rLayer2Layer = Layer <$> fmap rItem2Item . view gitems

-- | project to simple Page out of RPage
rPage2Page :: RPage -> Page
rPage2Page =
  Page <$> view gdimension
    <*> rbkg2Bkg . view gbackground
    <*> fmap rLayer2Layer . toList . view glayers

-- | project to simple Hoodle out of RHoodle
rHoodle2Hoodle :: RHoodle -> Hoodle
rHoodle2Hoodle =
  Hoodle <$> view ghoodleID
    <*> view gtitle
    <*> view grevisions
    <*> fmap pdfBase64 . view gembeddedpdf
    <*> view gembeddedtext
    <*> IM.elems . fmap rPage2Page . view gpages

----------------------
----- Rendering
----------------------

-- |
newtype InBBox a = InBBox {unInBBox :: a}

-- |
newtype InBBoxOption = InBBoxOption (Maybe BBox)

-- |
newtype InBBoxBkgBuf a = InBBoxBkgBuf {unInBBoxBkgBuf :: a}
