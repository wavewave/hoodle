{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.ModelAction.File 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.ModelAction.Text where

import           Control.Applicative
import           Data.Attoparsec.Text as A
import           Data.Char (isAlphaNum)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
--
import Debug.Trace

-- | 
getLinesFromText :: (Int,Int) -> T.Text -> T.Text
getLinesFromText (i,e) = T.unlines . Prelude.drop (i-1) . Prelude.take e . T.lines



-- |
getKeywordContent :: T.Text -- ^ keyword 
                  -> T.Text -- ^ tex file 
                  -> Maybe (T.Text, T.Text) -- [ (T.Text, T.Text) ] -- Maybe T.Text -- ^ subpart 
getKeywordContent k txt = 
    case parseOnly keywordContents txt of 
      Left err -> trace (show err) $ Nothing
      Right r -> Just r




keywordBegin :: A.Parser T.Text
keywordBegin = 
    string "%h%k " *> A.skipSpace 
                   *> A.takeWhile1 isAlphaNum 
                   <* A.skipWhile (not . isEndOfLine)
                   <* A.endOfLine

keywordEnd :: A.Parser ()
keywordEnd =  string "%h%k%end" >> skipWhile (notInClass "\r\n") >> endOfLine


-- |
oneline :: A.Parser T.Text
oneline = A.takeWhile (not . isEndOfLine) <* endOfLine
           
-- |
keywordContents :: A.Parser (T.Text, T.Text) -- ^ (keyword,contents)
keywordContents = do 
    k <- keywordBegin
    txt <- T.unlines <$> manyTill oneline keywordEnd
    return (k,txt)




