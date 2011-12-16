{-# LANGUAGE TypeFamilies #-}

module Data.Xournal.Generic where

import Data.IntMap hiding (map)
-- import Text.Xournal.Type
import Data.Xournal.Simple
import Data.ByteString hiding (map,zip)

import Control.Applicative
import Data.Functor

data GXournal s a = GXournal { gtitle :: ByteString 
                             , gpages :: s a }  

data GPage b s a = GPage { gdimension :: Dimension 
                         , gbackground :: b 
                         , glayers :: s a }  

data GLayer s a = GLayer { gstrokes :: s a } 

instance (Functor s) => Functor (GLayer s) where
  fmap f (GLayer strs) = GLayer (fmap f strs)
  
instance (Functor s) => Functor (GPage b s) where
  fmap f (GPage d b ls) = GPage d b (fmap f ls)
  
instance (Functor s) => Functor (GXournal s) where
  fmap f (GXournal t ps) = GXournal t (fmap f ps)



-- data GBackground b = GBackground b

data GSelect a b = GSelect { gselectAll :: a 
                           , gselectSelected :: b
                           }


type TLayerSimple = GLayer [] Stroke 

type TPageSimple = GPage Background [] TLayerSimple 

type TXournalSimple = GXournal [] TPageSimple 

class GStrokeable a where
  gFromStroke :: Stroke -> a 
  gToStroke :: a -> Stroke 
  
instance GStrokeable Stroke where
  gFromStroke = id
  gToStroke = id 


class GListable s where  
  gFromList :: [a] -> s a 
  gToList :: s a -> [a]
  
instance GListable [] where
  gFromList = id 
  gToList = id 
  
instance GListable IntMap where 
  gFromList = Data.IntMap.fromList . zip [0..] 
  gToList = Data.IntMap.elems 


class GBackgroundable b where
  gFromBackground :: Background -> b 
  gToBackground :: b -> Background
  
instance GBackgroundable Background where  
  gFromBackground = id 
  gToBackground = id

fromLayer :: (GStrokeable a, GListable s) => Layer -> GLayer s a 
fromLayer = GLayer . gFromList . fmap gFromStroke . layer_strokes 

fromPage :: (GStrokeable a, GBackgroundable b, 
             GListable s, GListable s') => 
            Page -> GPage b s' (GLayer s a)  
fromPage p = let bkg = gFromBackground $ page_bkg p 
                 dim = page_dim p 
                 ls =  gFromList . fmap fromLayer . page_layers $ p 
             in  GPage dim bkg ls 


class SListable m where
  chgStreamToList :: (GListable s) => m s a -> m [] a 
  
instance SListable GLayer where
  chgStreamToList (GLayer xs) = GLayer (gToList xs)

-- layerChangeStreamToList :: (GListable s) => GLayer s a -> GLayer [] a
-- layerChangeStreamToList (GLayer xs) = GLayer (gToList xs)
 
instance SListable (GPage b) where
  chgStreamToList (GPage d b ls) = GPage d b (gToList ls)
  
instance SListable GXournal where
  chgStreamToList (GXournal t ps) = GXournal t (gToList ps)
  


toLayer :: (GStrokeable a, GListable s) => GLayer s a -> Layer
toLayer = layerFromTLayerSimple . fmap gToStroke . chgStreamToList 

toPage :: (GStrokeable a, GBackgroundable b, GListable s, GListable s', Functor s') => 
          (b->Background) -> GPage b s' (GLayer s a) -> Page
toPage f = pageFromTPageSimple . bkgchange f . chgStreamToList . fmap (fmap gToStroke . chgStreamToList) 


bkgchange :: (b -> b') -> GPage b s a -> GPage b' s a 
bkgchange f p = p { gbackground = f (gbackground p) } 

mkTLayerSimpleFromLayer :: Layer -> TLayerSimple
mkTLayerSimpleFromLayer = GLayer <$> layer_strokes

mkTPageSimpleFromPage :: Page -> TPageSimple 
mkTPageSimpleFromPage = GPage <$> page_dim <*> page_bkg <*> map mkTLayerSimpleFromLayer . page_layers 

mkTXournalSimpleFromXournal :: Xournal -> TXournalSimple 
mkTXournalSimpleFromXournal = GXournal <$> xoj_title <*> map mkTPageSimpleFromPage . xoj_pages

layerFromTLayerSimple :: TLayerSimple -> Layer
layerFromTLayerSimple = Layer <$> gstrokes

pageFromTPageSimple :: TPageSimple -> Page
pageFromTPageSimple = Page <$> gdimension <*> gbackground <*> map layerFromTLayerSimple . glayers

xournalFromTXournalSimple :: TXournalSimple -> Xournal 
xournalFromTXournalSimple = Xournal <$> gtitle <*> map pageFromTPageSimple . gpages 

-- $(mkLabels [''GXournal, ''GPage, ''GLayer, ''GSelect])

{-


-}

{-
-}

{-
-}


{-
  
  
  
  
 -}               




