{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Xournal.Generic where

import Control.Category (id, (.))
import Data.ByteString.Char8 (ByteString)
import Data.IntMap
  ( IntMap,
    elems,
    fromList,
  )
import Data.Xournal.Simple
  ( Background (..),
    Dimension (..),
    Layer (..),
    Page (..),
    Stroke (..),
    Xournal (..),
  )
import Lens.Micro (Lens', lens, (^.))
import Prelude hiding (id, (.))

-- |
data GXournal s a = GXournal
  { gtitle :: ByteString,
    gpages :: s a
  }

-- |
data GPage b s a = GPage
  { gdimension :: Dimension,
    gbackground :: b,
    glayers :: s a
  }

-- |
data GLayer s a = GLayer {gstrokes :: s a}

-- |
data GLayerBuf b s a = GLayerBuf
  { gbuffer :: b,
    gbstrokes :: s a
  }

-- |
instance (Functor s) => Functor (GLayer s) where
  fmap f (GLayer strs) = GLayer (fmap f strs)

-- |
instance (Functor s) => Functor (GLayerBuf b s) where
  fmap f (GLayerBuf b strs) = GLayerBuf b (fmap f strs)

-- |
instance (Functor s) => Functor (GPage b s) where
  fmap f (GPage d b ls) = GPage d b (fmap f ls)

-- |
instance (Functor s) => Functor (GXournal s) where
  fmap f (GXournal t ps) = GXournal t (fmap f ps)

-- |
class GCast a b where
  gcast :: a -> b

-- |
data GSelect a b = GSelect
  { gselectTitle :: ByteString,
    gselectAll :: a,
    gselectSelected :: b
  }

-- |
type TLayerSimple = GLayer [] Stroke

-- |
type TPageSimple = GPage Background [] TLayerSimple

-- |
type TXournalSimple = GXournal [] TPageSimple

-- |
class GStrokeable a where
  gFromStroke :: Stroke -> a
  gToStroke :: a -> Stroke

-- |
instance GStrokeable Stroke where
  gFromStroke = id
  gToStroke = id

-- |
class GListable s where
  gFromList :: [a] -> s a
  gToList :: s a -> [a]

-- |
instance GListable [] where
  gFromList = id
  gToList = id

-- |
instance GListable IntMap where
  gFromList = Data.IntMap.fromList . zip [0 ..]
  gToList = Data.IntMap.elems

-- |
class GBackgroundable b where
  gFromBackground :: Background -> b
  gToBackground :: b -> Background

-- |
instance GBackgroundable Background where
  gFromBackground = id
  gToBackground = id

-- |
fromLayer :: (GStrokeable a, GListable s) => Layer -> GLayer s a
fromLayer = GLayer . gFromList . fmap gFromStroke . layer_strokes

-- |
fromPage ::
  ( GStrokeable a,
    GBackgroundable b,
    GListable s,
    GListable s'
  ) =>
  Page ->
  GPage b s' (GLayer s a)
fromPage p =
  let bkg = gFromBackground $ page_bkg p
      dim = page_dim p
      ls = gFromList . fmap fromLayer . page_layers $ p
   in GPage dim bkg ls

-- |
class SListable m where
  chgStreamToList :: (GListable s) => m s a -> m [] a

-- |
instance SListable GLayer where
  chgStreamToList (GLayer xs) = GLayer (gToList xs)

-- |
instance SListable (GPage b) where
  chgStreamToList (GPage d b ls) = GPage d b (gToList ls)

-- |
instance SListable GXournal where
  chgStreamToList (GXournal t ps) = GXournal t (gToList ps)

-- |
g_title :: Lens' (GXournal s a) ByteString
g_title = lens gtitle (\f a -> f {gtitle = a})

-- |
g_pages :: Lens' (GXournal s a) (s a)
g_pages = lens gpages (\f a -> f {gpages = a})

-- |
g_dimension :: Lens' (GPage b s a) Dimension
g_dimension = lens gdimension (\f a -> f {gdimension = a})

-- |
g_background :: Lens' (GPage b s a) b
g_background = lens gbackground (\f a -> f {gbackground = a})

-- |
g_layers :: Lens' (GPage b s a) (s a)
g_layers = lens glayers (\f a -> f {glayers = a})

-- |
g_strokes :: Lens' (GLayer s a) (s a)
g_strokes = lens gstrokes (\f a -> f {gstrokes = a})

-- |
g_bstrokes :: Lens' (GLayerBuf b s a) (s a)
g_bstrokes = lens gbstrokes (\f a -> f {gbstrokes = a})

-- |
g_buffer :: Lens' (GLayerBuf b s a) b
g_buffer = lens gbuffer (\f a -> f {gbuffer = a})

-- |
g_selectTitle :: Lens' (GSelect a b) ByteString
g_selectTitle = lens gselectTitle (\f a -> f {gselectTitle = a})

-- |
g_selectAll :: Lens' (GSelect a b) a
g_selectAll = lens gselectAll (\f a -> f {gselectAll = a})

-- |
g_selectSelected :: Lens' (GSelect a b) b
g_selectSelected = lens gselectSelected (\f a -> f {gselectSelected = a})

-- |
toLayer :: (GStrokeable a, GListable s) => GLayer s a -> Layer
toLayer = layerFromTLayerSimple . fmap gToStroke . chgStreamToList

-- |
toNoBufferLayer :: GLayerBuf b s a -> GLayer s a
toNoBufferLayer (GLayerBuf _b s) = GLayer s

-- |
toPage ::
  (GStrokeable a, GBackgroundable b, GListable s, GListable s', Functor s') =>
  (b -> Background) ->
  GPage b s' (GLayer s a) ->
  Page
toPage f = pageFromTPageSimple . bkgchange f . chgStreamToList . fmap (fmap gToStroke . chgStreamToList)

-- |
toPageFromBuf ::
  (GStrokeable a, GBackgroundable b, GListable s, GListable s', Functor s') =>
  (b -> Background) ->
  GPage b s' (GLayerBuf buf s a) ->
  Page
toPageFromBuf f = pageFromTPageSimple . bkgchange f . chgStreamToList . fmap (fmap gToStroke . chgStreamToList . toNoBufferLayer)

-- |
bkgchange :: (b -> b') -> GPage b s a -> GPage b' s a
bkgchange f p = p {gbackground = f (gbackground p)}

-- |
mkTLayerSimpleFromLayer :: Layer -> TLayerSimple
mkTLayerSimpleFromLayer = GLayer <$> layer_strokes

-- |
mkTPageSimpleFromPage :: Page -> TPageSimple
mkTPageSimpleFromPage = GPage <$> page_dim <*> page_bkg <*> map mkTLayerSimpleFromLayer . page_layers

-- |
mkTXournalSimpleFromXournal :: Xournal -> TXournalSimple
mkTXournalSimpleFromXournal = GXournal <$> xoj_title <*> map mkTPageSimpleFromPage . xoj_pages

-- |
layerFromTLayerSimple :: TLayerSimple -> Layer
layerFromTLayerSimple = Layer <$> gstrokes

-- |
pageFromTPageSimple :: TPageSimple -> Page
pageFromTPageSimple = Page <$> gdimension <*> gbackground <*> map layerFromTLayerSimple . glayers

-- |
xournalFromTXournalSimple :: TXournalSimple -> Xournal
xournalFromTXournalSimple = Xournal <$> gtitle <*> map pageFromTPageSimple . gpages

----

-- |
emptyPageFromOldPage :: (GListable s) => GPage b s a -> GPage b s a
emptyPageFromOldPage =
  GPage
    <$> (^. g_dimension)
    <*> (^. g_background)
    <*> pure (gFromList [])

--  (get g_dimension p) (get g_background p) (gFromList [] )

----

-- |
printLayerStructureInPage ::
  (GListable s) =>
  GPage b s (GLayerBuf buf [] a) ->
  IO ()
printLayerStructureInPage page = do
  let lyrs = page ^. g_layers
      lst = fmap (Prelude.length . (^. g_bstrokes)) (gToList lyrs)
  (Prelude.putStrLn . ("num of layers = " ++) . show . Prelude.length . gToList) lyrs
  Prelude.putStrLn $ "layer strokes = " ++ show lst
