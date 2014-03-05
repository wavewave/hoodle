{-# LANGUAGE CPP, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Text.Hoodle.Builder 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Text.Hoodle.Builder where

-- from other packages 
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Char8 (fromChar, fromString)
import           Control.Lens 
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Double.Conversion.ByteString (toFixed)
import           Data.Foldable (foldMap)
#if MIN_VERSION_base(4,5,0) 
import           Data.Monoid hiding ((<>)) 
#else
import           Data.Monoid 
#endif 
import           Data.Strict.Tuple
-- from hoodle platform 
import           Data.Hoodle.Simple
-- 

-- | 
(<>) :: Monoid a => a -> a -> a 
(<>) = mappend 

infixl 4 <>

-- | 
builder :: Hoodle -> L.ByteString
builder = toLazyByteString . buildHoodle

-- |
buildHoodle :: Hoodle -> Builder 
buildHoodle hdl = fromByteString "<?xml version=\"1.0\" standalone=\"no\"?>\n<hoodle version=\"0.2.999\" id=\""
                 <> fromByteString (view hoodleID hdl) 
                 <> fromByteString "\">\n" 
                 <> (buildTitle . view title) hdl 
                 <> (mconcat . map buildRevision . view revisions) hdl
                 <> (maybe mempty buildEmbeddedPdf . view embeddedPdf) hdl 
                 <> (mconcat . map buildPage . view pages) hdl
                 <> fromByteString "</hoodle>\n"
  
-- |                  
buildTitle :: S.ByteString -> Builder
buildTitle ttl = fromByteString "<title>"
                <> fromByteString ttl
                <> fromByteString "</title>\n"
                
-- |                 
buildRevision :: Revision -> Builder 
buildRevision Revision {..}     = fromByteString "<revision revmd5=\""
                                  <> fromByteString _revmd5
                                  <> fromByteString "\" revtxt=\""
                                  <> fromByteString _revtxt
                                  <> fromByteString "\"/>\n"
buildRevision RevisionInk {..}  = fromByteString "<revision revmd5=\""
                                  <> fromByteString _revmd5
                                  <> fromByteString "\" type=\"ink\">\n"
                                  <> foldMap buildStroke _revink
                                  <> fromByteString "</revision>\n"

-- | 
buildEmbeddedPdf :: S.ByteString -> Builder 
buildEmbeddedPdf pdf = fromByteString "<embeddedpdf src=\""
                       <> fromByteString pdf
                       <> fromByteString "\"/>\n"

-- | 
buildPage :: Page -> Builder 
buildPage pg = fromByteString "<page width=\""
              <> fromByteString (toFixed 2 w)
              <> fromByteString "\" height=\""
              <> fromByteString (toFixed 2 h)
              <> fromByteString "\">\n"   
              <> (buildBackground . view background) pg 
              <> (mconcat . map buildLayer . view layers) pg 
              <> fromByteString "</page>\n"
  where Dim w h = view dimension pg
  
-- | 
buildBackground :: Background -> Builder 
buildBackground bkg = 
  case bkg of  
    Background typ col sty -> 
      fromByteString "<background type=\""
      <> fromByteString typ
      <> fromByteString "\" color=\""
      <> fromByteString col
      <> fromByteString "\" style=\""
      <> fromByteString sty
      <> fromByteString "\"/>\n"
    BackgroundPdf typ mdom mfile pageno -> 
      fromByteString "<background type=\""
      <> fromByteString typ
      <> case mdom of 
           Nothing -> fromByteString  S.empty 
           Just dom -> fromByteString "\" domain=\""
                       <> fromByteString dom
      <> case mfile of 
           Nothing -> fromByteString S.empty 
           Just file ->  fromByteString "\" filename=\""
                         <> fromByteString file
      <> fromByteString "\" pageno=\""
      <> fromString (show pageno)
      <> fromByteString "\"/>\n"
    BackgroundEmbedPdf typ pageno -> 
      fromByteString "<background type=\""
      <> fromByteString typ 
      <> fromByteString "\" pageno=\""
      <> fromString (show pageno)
      <> fromByteString "\"/>\n"

      
-- | 
buildLayer :: Layer -> Builder
buildLayer layer = fromByteString "<layer>\n"
                  <> (mconcat . map buildItem . view items) layer
                  <> fromByteString "</layer>\n"

buildItem :: Item -> Builder
buildItem (ItemStroke strk) = buildStroke strk
buildItem (ItemImage img) = buildImage img
buildItem (ItemSVG svg) = buildSVG svg 
buildItem (ItemLink lnk) = buildLink lnk
buildItem (ItemAnchor anc) = buildAnchor anc

-- | 
buildStroke :: Stroke -> Builder
buildStroke stroke@(Stroke _ _ _ _) = 
    fromByteString "<stroke tool=\""
    <> fromByteString (stroke_tool stroke)
    <> fromByteString "\" color=\""
    <> fromByteString (stroke_color stroke)
    <> fromByteString "\" width=\""
    <> fromByteString (toFixed 2 (stroke_width stroke))
    <> fromByteString "\">\n"
    <> mconcat (map build2D (stroke_data stroke))
    <> fromByteString "\n</stroke>\n"
buildStroke stroke@(VWStroke _ _ _) =
    fromByteString "<stroke tool=\""
    <> fromByteString (stroke_tool stroke)
    <> fromByteString "\" color=\""
    <> fromByteString (stroke_color stroke)
    <> fromByteString "\" width=\""
    <> mconcat (map buildZFrm3D (stroke_vwdata stroke))
    <> fromByteString "\">\n"
    <> mconcat (map buildXYFrm3D (stroke_vwdata stroke))
    <> fromByteString "\n</stroke>\n"

buildImage :: Image -> Builder 
buildImage (Image bstr (x,y) (Dim w h)) =
    fromByteString "<img src=\""
    <> fromByteString bstr
    <> fromByteString "\" x=\"" 
    <> fromByteString (toFixed 2 x)
    <> fromByteString "\" y=\""
    <> fromByteString (toFixed 2 y)
    <> fromByteString "\" width=\""
    <> fromByteString (toFixed 2 w)
    <> fromByteString "\" height=\""
    <> fromByteString (toFixed 2 h)
    <> fromByteString "\" />\n"

buildSVG :: SVG -> Builder 
buildSVG (SVG mtxt mcmd rdr (x,y) (Dim w h)) =
    fromByteString "<svgobject x=\"" 
    <> fromByteString (toFixed 2 x)
    <> fromByteString "\" y=\""
    <> fromByteString (toFixed 2 y)
    <> fromByteString "\" width=\""
    <> fromByteString (toFixed 2 w)
    <> fromByteString "\" height=\""
    <> fromByteString (toFixed 2 h)
    <> fromByteString "\" >\n"
    <> maybe mempty (\txt->fromByteString "<text><![CDATA[" <> fromByteString txt <> fromByteString "]]></text>") mtxt 
    <> maybe mempty (\cmd->fromByteString "<command><![CDATA[" <> fromByteString cmd <> fromByteString "]]></command>") mcmd 
    <> fromByteString "<render><![CDATA[" 
    <> fromByteString rdr 
    <> fromByteString "]]></render>"
    <> fromByteString "</svgobject>\n"

buildLink :: Link -> Builder 
buildLink (Link i typ loc mtxt mcmd rdr (x,y) (Dim w h)) =
    fromByteString "<link id=\""  
    <> fromByteString i 
    <> fromByteString "\" type=\"" 
    <> fromByteString typ
    <> fromByteString "\" location=\""
    <> fromByteString loc
    <> fromByteString "\" x=\"" 
    <> fromByteString (toFixed 2 x)
    <> fromByteString "\" y=\""
    <> fromByteString (toFixed 2 y)
    <> fromByteString "\" width=\""
    <> fromByteString (toFixed 2 w)
    <> fromByteString "\" height=\""
    <> fromByteString (toFixed 2 h)
    <> fromByteString "\" >\n"
    <> maybe mempty (\txt->fromByteString "<text><![CDATA[" <> fromByteString txt <> fromByteString "]]></text>") mtxt 
    <> maybe mempty (\cmd->fromByteString "<command><![CDATA[" <> fromByteString cmd <> fromByteString "]]></command>") mcmd 
    <> fromByteString "<render><![CDATA[" 
    <> fromByteString rdr 
    <> fromByteString "]]></render>"
    <> fromByteString "</link>\n"
buildLink (LinkDocID i docid loc mtxt mcmd rdr (x,y) (Dim w h)) =
    fromByteString "<link id=\""  
    <> fromByteString i 
    <> fromByteString "\" type=\"linkdocid\" linkedid=\"" 
    <> fromByteString docid 
    <> fromByteString "\" location=\""
    <> fromByteString loc
    <> fromByteString "\" x=\"" 
    <> fromByteString (toFixed 2 x)
    <> fromByteString "\" y=\""
    <> fromByteString (toFixed 2 y)
    <> fromByteString "\" width=\""
    <> fromByteString (toFixed 2 w)
    <> fromByteString "\" height=\""
    <> fromByteString (toFixed 2 h)
    <> fromByteString "\" >\n"
    <> maybe mempty (\txt->fromByteString "<text><![CDATA[" <> fromByteString txt <> fromByteString "]]></text>") mtxt 
    <> maybe mempty (\cmd->fromByteString "<command><![CDATA[" <> fromByteString cmd <> fromByteString "]]></command>") mcmd 
    <> fromByteString "<render><![CDATA[" 
    <> fromByteString rdr 
    <> fromByteString "]]></render>"
    <> fromByteString "</link>\n"    
buildLink (LinkAnchor i docid loc anchorid (x,y) (Dim w h)) =
    fromByteString "<link id=\""  
    <> fromByteString i 
    <> fromByteString "\" type=\"linkdocid\" linkedid=\"" 
    <> fromByteString docid 
    <> fromByteString "\" location=\""
    <> fromByteString loc
    <> fromByteString "\" anchorid=\""
    <> fromByteString anchorid
    <> fromByteString "\" x=\"" 
    <> fromByteString (toFixed 2 x)
    <> fromByteString "\" y=\""
    <> fromByteString (toFixed 2 y)
    <> fromByteString "\" width=\""
    <> fromByteString (toFixed 2 w)
    <> fromByteString "\" height=\""
    <> fromByteString (toFixed 2 h)
    <> fromByteString "\" >\n"
    <> fromByteString "<render><![CDATA[" 
    <> fromByteString "]]></render>"
    <> fromByteString "</link>\n"    

buildAnchor :: Anchor -> Builder
buildAnchor (Anchor i (x,y) (Dim w h)) = 
    fromByteString "<anchor id=\""
    <> fromByteString i 
    <> fromByteString "\" x=\""
    <> fromByteString (toFixed 2 x)
    <> fromByteString "\" y=\""
    <> fromByteString (toFixed 2 y)
    <> fromByteString "\" width=\""
    <> fromByteString (toFixed 2 w)
    <> fromByteString "\" height=\""
    <> fromByteString (toFixed 2 h)    
    <> fromByteString "\" />\n"
             
-- | 
build2D :: Pair Double Double -> Builder 
build2D (x :!: y) = fromByteString (toFixed 2 x) 
                    <> fromChar ' ' 
                    <> fromByteString (toFixed 2 y) 
                    <> fromChar ' ' 
                      
-- |                       
buildXYFrm3D :: (Double,Double,Double) -> Builder 
buildXYFrm3D (x,y,_) =  fromByteString (toFixed 2 x) 
                        <> fromChar ' ' 
                        <> fromByteString (toFixed 2 y)
                        <> fromChar ' ' 
                         
-- |
buildZFrm3D :: (Double,Double,Double) -> Builder 
buildZFrm3D (_,_,z) = fromByteString (toFixed 2 z) 
                      <> fromChar ' '
