{-# LANGUAGE OverloadedStrings #-}

module Application.HXournal.Builder where

import Text.Xournal.Type
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8 (fromString, fromChar)

import Data.Monoid
import Data.Strict.Tuple

infixl 4 <>

(<>) :: Monoid a => a -> a -> a 
(<>) = mappend 


builder :: Xournal -> L.ByteString
builder = toLazyByteString . fromXournal

fromXournal :: Xournal -> Builder 
fromXournal xoj = fromByteString "<?xml version=\"1.0\" standalone=\"no\"?>\n<xournal version=\"0.4.2.1\">\n"
                  <> fromTitle (xoj_title xoj) <> mconcat (map fromPage (xoj_pages xoj))
                  <> fromByteString "</xournal>\n"
  
fromTitle :: S.ByteString -> Builder
fromTitle title = fromByteString "<title>"
                  <> fromByteString title
                  <> fromByteString "</title>\n"

  
fromPage :: Page -> Builder 
fromPage page = fromByteString "<page width=\""
                <> fromString (show w)
                <> fromByteString "\" height=\""
                <> fromString (show h)
                <> fromByteString "\">\n"   
                <> fromBackground (page_bkg page)
                <> mconcat (map fromLayer (page_layers page))
                <> fromByteString "</page>\n"
  where Dim w h = page_dim page
  
fromBackground :: Background -> Builder 
fromBackground bkg = fromByteString "<background type=\""
                     <> fromByteString (bkg_type bkg)
                     <> fromByteString "\" color=\""
                     <> fromByteString (bkg_color bkg)
                     <> fromByteString "\" style=\""
                     <> fromByteString (bkg_style bkg)
                     <> fromByteString "\"/>\n"
                     

fromLayer :: Layer -> Builder
fromLayer layer = fromByteString "<layer>\n"
                  <> mconcat (map fromStroke (layer_strokes layer))
                  <> fromByteString "</layer>\n"

fromStroke :: Stroke -> Builder
fromStroke stroke = fromByteString "<stroke tool=\""
                    <> fromByteString (stroke_tool stroke)
                    <> fromByteString "\" color=\""
                    <> fromByteString (stroke_color stroke)
                    <> fromByteString "\" width=\""
                    <> fromString (show (stroke_width stroke))
                    <> fromByteString "\">\n"
                    <> mconcat (map fromCoord (stroke_data stroke))
                    <> fromByteString "\n</stroke>\n"

fromCoord :: Pair Double Double -> Builder 
fromCoord (x :!: y) = fromString (show x) 
                      <> fromChar ' ' 
                      <> fromString (show y) 
                      <> fromChar ' ' 
