module Graphics.Xournal.Type.Generic where

import Data.IntMap 
import Text.Xournal.Type
import Graphics.Xournal.Type 

data GXournal s a = GXournal (s a)  

data GPage b s a = GPage Dimension b (s a)  

data GLayer s a = GLayer (s a)

data GBackground b = GBackground b

data GSelect a b = GSelect a b


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

type TPageBBoxMapBuffer b = GPage (GBackground b) IntMap TLayerBBox

type TXournalBBoxMapBuffer b = GXournal IntMap (TPageBBoxMapBuffer b)



type TAlterHitted a = AlterList [a] [a]

newtype TEitherAlterHitted a = 
          TEitherAlterHitted { 
            unTEitherAlterHitted :: Either [a] (TAlterHitted a)
          }


type TLayerSelect a = GLayer TEitherAlterHitted a 

data TLayerSelectInPage s a = TLayerSelectInPage (TLayerSelect a) (s a)

type TTempPageSelect = GPage Background (TLayerSelectInPage []) TLayerBBox
                       
type TTempXournalSelect = GSelect (IntMap TPageBBoxMap) (Maybe (Int, TTempPageSelect))



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

gbkgbuffer :: GBackground b -> b 
gbkgbuffer (GBackground buf) = buf

gselectGetAll :: GSelect a b -> a 
gselectGetAll (GSelect xs _) = xs 

gselectGetSelected :: GSelect a b -> b 
gselectGetSelected (GSelect _ sel) = sel

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
                 