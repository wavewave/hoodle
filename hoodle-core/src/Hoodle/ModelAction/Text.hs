{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.ModelAction.File 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.ModelAction.Text where

import           Control.Applicative
import           Data.Attoparsec.Text as A
import           Data.Char (isAlphaNum)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
--
import Debug.Trace

-- | 
getLinesFromText :: (Int,Int) -> T.Text -> T.Text
getLinesFromText (i,e) = T.unlines . Prelude.drop (i-1) . Prelude.take e . T.lines


{-
-- |
getKeywordContent :: T.Text -- ^ keyword 
                  -> T.Text -- ^ tex file 
                  -> Maybe T.Text -- ^ subpart 
getKeywordContent k txt = M.lookup k (getKeywordMap txt)
-}

-- | 
getKeywordMap :: T.Text -> M.HashMap T.Text T.Text
getKeywordMap txt = case parseOnly (many keywordContents) txt of 
                      Left err -> trace (show err) $ M.empty
                      Right lst -> M.fromList lst


-- |
keywordBegin :: A.Parser T.Text
keywordBegin = 
    skipWhile (notInClass "%") *> (try (string "%h%k " *> A.skipSpace 
                                        *> A.takeWhile1 isAlphaNum 
                                        <* A.skipWhile (not . isEndOfLine)
                                        <* A.endOfLine)
                                   <|> (oneline *> keywordBegin))

-- | 
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

-- |
extractKeyword :: T.Text -> Maybe T.Text
extractKeyword txt = 
    case T.unpack txt of
      'e':'m':'b':'e':'d':'l':'a':'t':'e':'x':':':'k':'e':'y':'w':'o':'r':'d':':':xs -> Just (T.pack xs)
      _ -> Nothing


