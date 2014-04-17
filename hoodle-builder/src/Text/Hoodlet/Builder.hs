{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Text.Hoodlet.Builder 
-- Copyright   : (c) 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Text.Hoodlet.Builder where

import           Blaze.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as L
-- 
import           Data.Hoodle.Simple
--
import           Text.Hoodle.Builder
--
-- | 
builder :: Item -> L.ByteString
builder = toLazyByteString . buildHoodlet


buildHoodlet :: Item -> Builder
buildHoodlet item = fromByteString "<?xml version=\"1.0\" standalone=\"no\"?>\n<hoodlet version=\"0.2.2\" >\n" 
                    <> buildItem item
                    <> fromByteString "</hoodlet>\n"

