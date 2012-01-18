{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Text.Xournal.Builder 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Text.Xournal.Builder where

import Data.Xournal.Simple
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8 (fromChar, fromString)
import Data.Double.Conversion.ByteString 
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
                <> fromByteString (toFixed 2 w)
                <> fromByteString "\" height=\""
                <> fromByteString (toFixed 2 h)
                <> fromByteString "\">\n"   
                <> fromBackground (page_bkg page)
                <> mconcat (map fromLayer (page_layers page))
                <> fromByteString "</page>\n"
  where Dim w h = page_dim page
  
fromBackground :: Background -> Builder 
fromBackground bkg = 
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
                    <> fromByteString (toFixed 2 (stroke_width stroke))
                    <> fromByteString "\">\n"
                    <> mconcat (map fromCoord (stroke_data stroke))
                    <> fromByteString "\n</stroke>\n"

fromCoord :: Pair Double Double -> Builder 
fromCoord (x :!: y) = fromByteString (toFixed 2 x) 
                      <> fromChar ' ' 
                      <> fromByteString (toFixed 2 y) 
                      <> fromChar ' ' 
