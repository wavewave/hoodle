{-# LANGUAGE TypeFamilies #-}

module Graphics.Xournal.Type.Generic where

import Data.IntMap 
import Text.Xournal.Type
import Graphics.Xournal.Type 

import Control.Category
import Data.Label
import Prelude hiding ((.),id)

import Data.Functor

data GXournal s a = GXournal { gpages :: s a }  

data GPage b s a = GPage { gdimension :: Dimension 
                         , gbackground :: b 
                         , glayers :: s a }  

data GLayer s a = GLayer { gstrokes :: s a } 

instance (Functor s) => Functor (GLayer s) where
  fmap f (GLayer strs) = GLayer (fmap f strs)
  
instance (Functor s) => Functor (GPage b s) where
  fmap f (GPage d b ls) = GPage d b (fmap f ls)
  
instance (Functor s) => Functor (GXournal s) where
  fmap f (GXournal ps) = GXournal (fmap f ps)

bkgchange :: (b -> b') -> GPage b s a -> GPage b' s a 
bkgchange f p = p { gbackground = f (gbackground p) } 


-- data GBackground b = GBackground b

data GSelect a b = GSelect { gselectAll :: a 
                           , gselectSelected :: b
                           }

-- $(mkLabels [''GXournal, ''GPage, ''GLayer, ''GSelect])

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



type TAlterHitted a = AlterList [a] (Hitted a)

newtype TEitherAlterHitted a = 
          TEitherAlterHitted { 
            unTEitherAlterHitted :: Either [a] (TAlterHitted a)
          }


type TLayerSelect a = GLayer TEitherAlterHitted (StrokeTypeFromLayer a) 

type family StrokeTypeFromLayer a  :: * 
     
type instance StrokeTypeFromLayer TLayerBBox = StrokeBBox
 

data TLayerSelectInPage s a = TLayerSelectInPage 
                              { gselectedlayer :: TLayerSelect a 
                              , gotherlayers :: s a
                              }

type TTempPageSelect = GPage Background (TLayerSelectInPage []) TLayerBBox
                       
type TTempXournalSelect = GSelect (IntMap TPageBBoxMap) (Maybe (Int, TTempPageSelect))


{-
gpages :: GXournal s a -> s a 
gpages (GXournal lst) = lst

gdimension :: GPage b s a -> Dimension
gdimension (GPage dim  _ _) = dim 

gbackground :: GPage b s a -> b 
gbackground (GPage _ b _) = b

glayers :: GPage b s a -> s a 
glayers (GPage _ _ lst) = lst

gstrokes :: GLayer s a -> s a 
gstrokes (GLayer lst) = lst 

-}
{-
gbkgbuffer :: GBackground b -> b 
gbkgbuffer (GBackground buf) = buf
-}
{-
gselectAll :: GSelect a b -> a 
gselectAll (GSelect xs _) = xs 

gselectSelected :: GSelect a b -> b 
gselectSelected (GSelect _ sel) = sel
-}


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
                 




