{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Hoodle.Generic 
-- Copyright   : (c) 2011,2012,2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Data.Hoodle.Generic where

-- from other packages
import Control.Category
import Control.Lens 
import Data.ByteString.Char8 hiding (map,zip)
import Data.Foldable
import Data.Functor
import qualified Data.IntMap as IM 
import qualified Data.Sequence as Seq 
import qualified Data.Text as T
import Data.UUID.V4 
-- from this package
import Data.Hoodle.Simple
-- 
import Prelude hiding ((.),id)

data PDFData = PDFData { pdfBase64 :: ByteString 
                       , pdfNumPages :: Int } 


-- | Generic Hoodle data having generic pages
data GHoodle cntnr pg = GHoodle 
                        { ghoodle_id :: ByteString 
                        , ghoodle_ttl :: ByteString 
                        , ghoodle_revisions :: [Revision]
                        , ghoodle_embeddedpdf :: Maybe PDFData -- Maybe ByteString 
                        , ghoodle_embeddedtext :: Maybe T.Text
                        , ghoodle_pgs :: cntnr pg }  

-- | Generic page data having dimension, generic background
--   and generic layers
data GPage bkg cntnr lyr = GPage 
                           { gpage_dim :: Dimension 
                           , gpage_bkg :: bkg 
                           , gpage_lyrs :: cntnr lyr }  

-- | Generic buffered layer having generic items
data GLayer buf cntnr itm = GLayer 
                             { glayer_buf :: buf 
                             , glayer_itms :: cntnr itm 
                             } 
                       
-- | 
instance (Functor cntnr) => Functor (GLayer buf cntnr) where
  fmap f (GLayer buf itms) = GLayer buf (fmap f itms) 
  
-- | 
instance (Functor cntnr) => Functor (GPage bkg cntnr) where
  fmap f (GPage dim bkg lyrs) = GPage dim bkg (fmap f lyrs)
  
-- | 
instance (Functor cntnr) => Functor (GHoodle cntnr) where
  fmap f (GHoodle hid ttl revs pdf txt pgs) = GHoodle hid ttl revs pdf txt (fmap f pgs)

------------------------------
-- lenses for Generic types --
------------------------------

-- | 
ghoodleID :: Simple Lens (GHoodle cntnr pg) ByteString 
ghoodleID = lens ghoodle_id (\f a -> f { ghoodle_id = a } )

-- |
gtitle :: Simple Lens (GHoodle cntnr pg) ByteString
gtitle = lens ghoodle_ttl (\f a -> f { ghoodle_ttl = a } )

-- | 
grevisions :: Simple Lens (GHoodle cntnr pg) [Revision]
grevisions = lens ghoodle_revisions (\f a -> f { ghoodle_revisions = a } )

-- | 
gembeddedpdf :: Simple Lens (GHoodle cntnr pg) (Maybe PDFData)
gembeddedpdf = lens ghoodle_embeddedpdf (\f a -> f { ghoodle_embeddedpdf = a } )

-- | 
gembeddedtext :: Simple Lens (GHoodle cntnr pg) (Maybe T.Text)
gembeddedtext = lens ghoodle_embeddedtext (\f a -> f { ghoodle_embeddedtext = a } )

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
  
-- |
instance Listable [] where
  fromList = id 
  
-- | 
instance Listable IM.IntMap where 
  fromList = IM.fromList . zip [0..] 
  
-- | 
instance Listable Seq.Seq where
  fromList = Seq.fromList 

-- |
emptyGHoodle :: (Listable m) => IO (GHoodle m a)
emptyGHoodle = do 
  uuid <- nextRandom
  return $ GHoodle ((pack.show) uuid) "" [] Nothing Nothing (fromList [])

-- | 
emptyGPage :: (Listable cntnr) => Dimension -> bkg -> GPage bkg cntnr a 
emptyGPage dim bkg = GPage dim bkg (fromList [])
