{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Text.Hoodlet.Parse.Attoparsec where

import Control.Applicative
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
--
import qualified Data.Hoodle.Simple as H
--
import Text.Hoodle.Parse.Attoparsec

-- |
hoodlet :: Parser H.Item
hoodlet = do
  trim
  xmlheader <?> "xmlheader"
  trim
  hoodletheader <?> "hoodletheader"
  trim
  itm <- try (H.ItemStroke <$> onestroke) <|> try img <|> try svgObj <|> link
  trim
  string "</hoodlet>"
  return itm

hoodletheader :: Parser B.ByteString
hoodletheader = do
  string "<hoodlet"
  trim
  v <- hoodleversion
  trim
  char '>'
  return v
