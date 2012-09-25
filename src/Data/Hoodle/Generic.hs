{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Hoodle.Generic 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Data.Hoodle.Generic where

-- from other packages
-- import Control.Applicative
import Control.Category
import Control.Lens 
import Data.ByteString hiding (map,zip)
import Data.Foldable
import Data.Functor
import qualified Data.IntMap as IM --  hiding (map)
import qualified Data.Sequence as Seq 
-- from this package
import Data.Hoodle.Simple
-- 
import Prelude hiding ((.),id)

-- | Generic Hoodle data having generic pages
data GHoodle cntnr pg = GHoodle 
                        { ghoodle_ttl :: ByteString 
                        , ghoodle_pgs :: cntnr pg }  

-- | Generic page data having dimension, generic background
--   and generic layers
data GPage bkg cntnr lyr = GPage 
                           { gpage_dim :: Dimension 
                           , gpage_bkg :: bkg 
                           , gpage_lyrs :: cntnr lyr }  

-- -- | Generic layer data having generic items 
-- data GLayer cntnr itm = GLayer { glayer_itms :: cntnr itm } 
            
-- | Generic buffered layer having generic items
data GLayer buf cntnr itm = GLayer 
                             { glayer_buf :: buf 
                             -- , glayer_strks :: cntnr strk 
                             , glayer_itms :: cntnr itm 
                             } 
                       
{-                          
-- |
instance (Functor s) => Functor (GLayer s) where
  fmap f (GLayer bstrs) = GLayer (fmap f strs)
-}

-- | 
instance (Functor cntnr) => Functor (GLayer buf cntnr) where
  fmap f (GLayer buf itms) = GLayer buf (fmap f itms) 
  
-- | 
instance (Functor cntnr) => Functor (GPage bkg cntnr) where
  fmap f (GPage dim bkg lyrs) = GPage dim bkg (fmap f lyrs)
  
-- | 
instance (Functor cntnr) => Functor (GHoodle cntnr) where
  fmap f (GHoodle ttl pgs) = GHoodle ttl (fmap f pgs)

------------------------------
-- lenses for Generic types --
------------------------------


-- |
gtitle :: Simple Lens (GHoodle cntnr pg) ByteString
gtitle = lens ghoodle_ttl (\f a -> f { ghoodle_ttl = a } )

-- |
gpages :: Simple Lens (GHoodle cntnr pg) (cntnr pg)
gpages = lens ghoodle_pgs (\f a -> f { ghoodle_pgs = a } )

-- |
gdimension :: Simple Lens (GPage bkg cntnr pg) Dimension 
gdimension = lens gpage_dim (\f a -> f { gpage_dim = a } )

-- |
gbackground :: Simple Lens (GPage bkg cntnr lyr) bkg 
gbackground = lens gpage_bkg (\f a -> f { gpage_bkg = a } ) 

-- |
glayers :: Simple Lens (GPage bkg cntnr lyr) (cntnr lyr)
glayers = lens gpage_lyrs (\f a -> f { gpage_lyrs = a } ) 


-- |
gitems :: Simple Lens (GLayer buf cntnr itm) (cntnr itm)
gitems = lens glayer_itms (\f a -> f { glayer_itms = a } )


{-
-- |
gstrokes :: Simple Lens (GLayer buf cntnr strk) (cntnr strk)
gstrokes = lens glayer_strks (\f a -> f { glayer_strks = a } )
-}

-- |
gbuffer :: Simple Lens (GLayer buf cntnr itm) buf 
gbuffer = lens glayer_buf (\f a -> f { glayer_buf = a } )



-- |
class (Foldable s) => Listable s where  
  fromList :: [a] -> s a 
--   toList :: s a -> [a]
  
-- |
instance Listable [] where
  fromList = id 
--   toList = id 
  
-- | 
instance Listable IM.IntMap where 
  fromList = IM.fromList . zip [0..] 
--   toList = Data.IntMap.elems 
  
-- | 
instance Listable Seq.Seq where
  fromList = Seq.fromList 

-- |
emptyGHoodle :: (Listable m) => GHoodle m a
emptyGHoodle = GHoodle "" (fromList [])

-- | 
emptyGPage :: (Listable cntnr) => Dimension -> bkg -> GPage bkg cntnr a 
emptyGPage dim bkg = GPage dim bkg (fromList [])
  

{-
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
fromPage :: (GStrokeable a, GBackgroundable b, 
             GListable s, GListable s') => 
            Page -> GPage b s' (GLayer s a)  
fromPage p = let bkg = gFromBackground $ page_bkg p 
                 dim = page_dim p 
                 ls =  gFromList . fmap fromLayer . page_layers $ p 
             in  GPage dim bkg ls 

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
instance SListable GHoodle where
  chgStreamToList (GHoodle t ps) = GHoodle t (gToList ps)
  

-- |
toLayer :: (GStrokeable a, GListable s) => GLayer s a -> Layer
toLayer = layerFromTLayerSimple . fmap gToStroke . chgStreamToList 

-- |
toNoBufferLayer :: GLayerBuf b s a -> GLayer s a 
toNoBufferLayer (GLayerBuf _b s) = GLayer s 

-- | 
toPage :: (GStrokeable a, GBackgroundable b, GListable s, GListable s', Functor s') => 
          (b->Background) -> GPage b s' (GLayer s a) -> Page
toPage f = pageFromTPageSimple . bkgchange f . chgStreamToList . fmap (fmap gToStroke . chgStreamToList) 

-- | 
toPageFromBuf :: (GStrokeable a, GBackgroundable b, GListable s, GListable s', Functor s') => 
              (b->Background) -> GPage b s' (GLayerBuf buf s a) -> Page
toPageFromBuf f = pageFromTPageSimple . bkgchange f . chgStreamToList . fmap (fmap gToStroke . chgStreamToList . toNoBufferLayer) 

-- | 
bkgchange :: (b -> b') -> GPage b s a -> GPage b' s a 
bkgchange f p = p { gbackground = f (gbackground p) } 


-- | 
mkTPageSimpleFromPage :: Page -> TPageSimple 
mkTPageSimpleFromPage = GPage <$> page_dim <*> page_bkg <*> map mkTLayerSimpleFromLayer . page_layers 

-- | 
mkTHoodleSimpleFromHoodle :: Hoodle -> THoodleSimple 
mkTHoodleSimpleFromHoodle = 
  GHoodle <$> hoodle_title 
          <*> map mkTPageSimpleFromPage . hoodle_pages

-- | 
layerFromTLayerSimple :: TLayerSimple -> Layer
layerFromTLayerSimple = Layer <$> gstrokes

-- | 
pageFromTPageSimple :: TPageSimple -> Page
pageFromTPageSimple = Page <$> gdimension <*> gbackground <*> map layerFromTLayerSimple . glayers

-- | 
hoodleFromTHoodleSimple :: THoodleSimple -> Hoodle 
hoodleFromTHoodleSimple = Hoodle <$> gtitle <*> map pageFromTPageSimple . gpages 


---- 

-- |
emptyPageFromOldPage :: (GListable s) => GPage b s a -> GPage b s a
emptyPageFromOldPage = GPage 
                       <$> (^.g_dimension) 
                       <*> (^.g_background) 
                       <*> pure (gFromList []) 
--  (get g_dimension p) (get g_background p) (gFromList [] )

----

-- | 
printLayerStructureInPage :: (GListable s) => 
                              GPage b s (GLayerBuf buf [] a) -> IO () 
printLayerStructureInPage page = do 
  let lyrs = page^.g_layers 
      lst = fmap (Prelude.length . (^.g_bstrokes)) (gToList lyrs)
  (Prelude.putStrLn . ("num of layers = "++) . show . Prelude.length . gToList ) lyrs 
  Prelude.putStrLn $ "layer strokes = " ++ show lst 
-}

{-
-- | 
type TLayerSimple = GLayer [] Stroke 

-- | 
type TPageSimple = GPage Background [] TLayerSimple 

-- |
type THoodleSimple = GHoodle [] TPageSimple 
-}

{-
-- |
class GStrokeable a where
  gFromStroke :: Stroke -> a 
  gToStroke :: a -> Stroke 
  
-- |
instance GStrokeable Stroke where
  gFromStroke = id
  gToStroke = id 
-}

{-
-- | 
class GCast a b where 
  gcast :: a -> b 
-}
