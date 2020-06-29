{-# LANGUAGE TypeFamilies, TypeOperators, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Hoodle.Select 
-- Copyright   : (c) 2011,2012,2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Data.Hoodle.Select where

-- from other packages
import           Control.Applicative
import           Data.ByteString
import qualified Data.Text as T
import           Lens.Micro
import           Lens.Micro.Extras (view)
--
import           Data.Hoodle.Generic 
import           Data.Hoodle.Simple 

-- | 
data GSelect a b = GSelect { gselect_id :: ByteString 
                           , gselect_ttl :: ByteString 
                           , gselect_revisions :: [Revision]
                           , gselect_embeddedpdf :: Maybe PDFData -- Maybe ByteString
                           , gselect_embeddedtext :: Maybe T.Text
                           , gselect_all :: a 
                           , gselect_selected :: b
                           }

gselHoodleID :: Lens' (GSelect a b) ByteString 
gselHoodleID = lens gselect_id (\f a -> f { gselect_id = a } )

-- |
gselTitle :: Lens' (GSelect a b) ByteString
gselTitle = lens gselect_ttl (\f a -> f {gselect_ttl = a})

-- | 
gselRevisions :: Lens' (GSelect a b) [Revision]
gselRevisions = lens gselect_revisions (\f a -> f {gselect_revisions = a } )

-- |
gselEmbeddedPdf :: Lens' (GSelect a b) (Maybe PDFData)
gselEmbeddedPdf = lens gselect_embeddedpdf (\f a -> f {gselect_embeddedpdf = a})

-- |
gselEmbeddedText :: Lens' (GSelect a b) (Maybe T.Text)
gselEmbeddedText = lens gselect_embeddedtext (\f a -> f {gselect_embeddedtext = a})

-- |
gselAll :: Lens' (GSelect a b) a 
gselAll = lens gselect_all (\f a -> f {gselect_all = a} )

-- |
gselSelected :: Lens' (GSelect a b) b
gselSelected = lens gselect_selected (\f a -> f {gselect_selected = a})


gSelect2GHoodle :: GSelect (m a) b -> GHoodle m a 
gSelect2GHoodle = GHoodle <$> view gselHoodleID
                          <*> view gselTitle 
                          <*> view gselRevisions
                          <*> view gselEmbeddedPdf 
                          <*> view gselEmbeddedText
                          <*> view gselAll 


gHoodle2GSelect :: GHoodle m a -> GSelect (m a) (Maybe b)
gHoodle2GSelect = GSelect <$> view ghoodleID
                          <*> view gtitle 
                          <*> view grevisions
                          <*> view gembeddedpdf
                          <*> view gembeddedtext
                          <*> view gpages
                          <*> pure Nothing 
