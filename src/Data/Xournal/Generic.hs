{-# LANGUAGE TypeFamilies #-}

module Data.Xournal.Generic where

-- import Data.IntMap 
-- import Text.Xournal.Type
import Data.Xournal.Simple
import Data.ByteString hiding (map)

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
type TLayerSimple = GLayer [] Stroke 

type TPageSimple = GPage Background [] TLayerSimple 

type TXournalSimple = GXournal [] TPageSimple 

type TPageMap = GPage Background IntMap TLayerSimple 

type TXournalMap = GXournal [] TPageMap 

type TLayerBBox = GLayer [] StrokeBBox 

type TPageBBox = GPage Background [] TLayerBBox 

type TXournalBBox = GXournal [] TPageBBox

type TPageBBoxMap = GPage Background IntMap TLayerBBox

type TXournalBBoxMap = GXournal IntMap TPageBBoxMap

type TPageBBoxMapBkg b = GPage b IntMap TLayerBBox

type TXournalBBoxMapBkg b = GXournal IntMap (TPageBBoxMapBkg b)
-}

{-
type TAlterHitted a = AlterList [a] (Hitted a)

newtype TEitherAlterHitted a = 
          TEitherAlterHitted { 
            unTEitherAlterHitted :: Either [a] (TAlterHitted a)
          }
-}

{-
type TLayerSelect a = GLayer TEitherAlterHitted (StrokeTypeFromLayer a) 

type family StrokeTypeFromLayer a  :: * 
     
type instance StrokeTypeFromLayer TLayerBBox = StrokeBBox
 

data TLayerSelectInPage s a = TLayerSelectInPage 
                              { gselectedlayer :: TLayerSelect a 
                              , gotherlayers :: s a
                              }

type TTempPageSelect = GPage Background (TLayerSelectInPage []) TLayerBBox
                       
type TTempXournalSelect = GSelect (IntMap TPageBBoxMap) (Maybe (Int, TTempPageSelect))
-}


{-
class GStrokeable a where
  gFromStroke :: Stroke -> a 
  gToStroke :: a -> Stroke 
  
instance GStrokeable Stroke where
  gFromStroke = id
  gToStroke = id 
  
instance GStrokeable StrokeBBox where  
  gFromStroke = mkStrokeBBoxFromStroke 
  gToStroke = strokeFromStrokeBBox
  
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
fromLayer = GLayer . gFromList . Prelude.map gFromStroke . layer_strokes 

fromPage :: (GStrokeable a, GBackgroundable b, 
             GListable s, GListable s') => 
            Page -> GPage b s' (GLayer s a)  
fromPage p = let bkg = gFromBackground $ page_bkg p 
                 dim = page_dim p 
                 ls =  gFromList . Prelude.map fromLayer . page_layers $ p 
             in  GPage dim bkg ls 
 -}               




