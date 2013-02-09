{-# LANGUAGE TypeFamilies, TypeOperators, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Hoodle.Select 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Data.Hoodle.Select where

-- from other packages
import Control.Applicative
import Control.Lens
import Data.ByteString 
--
import Data.Hoodle.Generic 

-- | 
data GSelect a b = GSelect { gselect_id :: ByteString 
                           , gselect_ttl :: ByteString 
                           , gselect_embeddedpdf :: Maybe ByteString 
                           , gselect_all :: a 
                           , gselect_selected :: b
                           }

gselHoodleID :: Simple Lens (GSelect a b) ByteString 
gselHoodleID = lens gselect_id (\f a -> f { gselect_id = a } )

-- |
gselTitle :: Simple Lens (GSelect a b) ByteString
gselTitle = lens gselect_ttl (\f a -> f {gselect_ttl = a})

-- |
gselEmbeddedPdf :: Simple Lens (GSelect a b) (Maybe ByteString)
gselEmbeddedPdf = lens gselect_embeddedpdf (\f a -> f {gselect_embeddedpdf = a})

-- |
gselAll :: Simple Lens (GSelect a b) a 
gselAll = lens gselect_all (\f a -> f {gselect_all = a} )

-- |
gselSelected :: Simple Lens (GSelect a b) b
gselSelected = lens gselect_selected (\f a -> f {gselect_selected = a})


gSelect2GHoodle :: GSelect (m a) b -> GHoodle m a 
gSelect2GHoodle = GHoodle <$> view gselHoodleID
                          <*> view gselTitle 
                          <*> view gselEmbeddedPdf 
                          <*> view gselAll 


gHoodle2GSelect :: GHoodle m a -> GSelect (m a) (Maybe b)
gHoodle2GSelect = GSelect <$> view ghoodleID
                          <*> view gtitle 
                          <*> view gembeddedpdf
                          <*> view gpages
                          <*> pure Nothing 