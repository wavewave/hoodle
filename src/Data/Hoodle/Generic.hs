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
                        , ghoodle_embeddedpdf :: Maybe ByteString 
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
  fmap f (GHoodle ttl pdf pgs) = GHoodle ttl pdf (fmap f pgs)

------------------------------
-- lenses for Generic types --
------------------------------


-- |
gtitle :: Simple Lens (GHoodle cntnr pg) ByteString
gtitle = lens ghoodle_ttl (\f a -> f { ghoodle_ttl = a } )

-- | 
gembeddedpdf :: Simple Lens (GHoodle cntnr pg) (Maybe ByteString)
gembeddedpdf = lens ghoodle_embeddedpdf (\f a -> f { ghoodle_embeddedpdf = a } )

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
emptyGHoodle = GHoodle "" Nothing (fromList [])

-- | 
emptyGPage :: (Listable cntnr) => Dimension -> bkg -> GPage bkg cntnr a 
emptyGPage dim bkg = GPage dim bkg (fromList [])
  

