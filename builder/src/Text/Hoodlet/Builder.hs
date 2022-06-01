{-# LANGUAGE OverloadedStrings #-}

module Text.Hoodlet.Builder where

import Blaze.ByteString.Builder (Builder, fromByteString, toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as L
--
import Data.Hoodle.Simple (Item)
--
import Text.Hoodle.Builder (buildItem)

-- |
builder :: Item -> L.ByteString
builder = toLazyByteString . buildHoodlet

buildHoodlet :: Item -> Builder
buildHoodlet item =
  fromByteString "<?xml version=\"1.0\" standalone=\"no\"?>\n<hoodlet version=\"0.2.2\" >\n"
    <> buildItem item
    <> fromByteString "</hoodlet>\n"
