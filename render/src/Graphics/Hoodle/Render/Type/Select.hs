{-# LANGUAGE TypeFamilies #-}

module Graphics.Hoodle.Render.Type.Select where

import Control.Lens (Lens', lens, view)
import Data.Hoodle.Generic
  ( GLayer (..),
    GPage (..),
    gbackground,
    gbuffer,
    gdimension,
    gitems,
    glayers,
  )
import Data.Hoodle.Select (GSelect)
import Data.Hoodle.Zipper (ZipperSelect, current, replace)
import Data.IntMap (IntMap)
import Data.Kind (Type)
import Graphics.Hoodle.Render.Type.Background (RBackground)
import Graphics.Hoodle.Render.Type.HitTest
  ( Hitted (..),
    TEitherAlterHitted (..),
    interleave,
  )
import Graphics.Hoodle.Render.Type.Hoodle (RLayer, RPage)
import Graphics.Hoodle.Render.Type.Item (RItem)

----------------------------
-- select state rendering --
----------------------------

type SLayerF a = GLayer (BufOf a) TEitherAlterHitted (ItmOf a)

type family ItmOf a :: Type

type family BufOf a :: Type

type instance BufOf (GLayer b s a) = b

type instance ItmOf RLayer = RItem

data HLayersF s a = HLayersF
  { hlyrt_selectedLayer :: SLayerF a,
    hlyrt_otherLayers :: s a
  }

type HLayers = HLayersF ZipperSelect RLayer

type HLayer = SLayerF RLayer

selectedLayer :: Lens' HLayers HLayer
selectedLayer = lens hlyrt_selectedLayer (\f a -> f {hlyrt_selectedLayer = a})

otherLayers :: Lens' HLayers (ZipperSelect RLayer)
otherLayers = lens hlyrt_otherLayers (\f a -> f {hlyrt_otherLayers = a})

-- |
type HPage =
  GPage RBackground (HLayersF ZipperSelect) RLayer

-- |
type HHoodle =
  GSelect (IntMap RPage) (Maybe (Int, HPage))

-- |
hLayer2RLayer :: HLayer -> RLayer
hLayer2RLayer l =
  case unTEitherAlterHitted (view gitems l) of
    Left strs -> GLayer (view gbuffer l) strs
    Right alist ->
      GLayer (view gbuffer l) . Prelude.concat $
        interleave id unHitted alist

-- |
hPage2RPage :: HPage -> RPage
hPage2RPage p =
  let HLayersF s others = view glayers p
      s' = hLayer2RLayer s
   in GPage (view gdimension p) (view gbackground p) (replace s' others)

-- |
mkHPage :: RPage -> HPage
mkHPage p =
  let sz = view glayers p
      curr = current sz
      currtemp = GLayer (view gbuffer curr) (TEitherAlterHitted . Left . view gitems $ curr)
   in GPage
        (view gdimension p)
        (view gbackground p)
        (HLayersF currtemp sz)
