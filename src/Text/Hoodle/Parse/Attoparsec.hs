{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Text.Hoodle.Parse.Attoparsec
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- attoparsec implementation of hoodle parser
-- 
-----------------------------------------------------------------------------

module Text.Hoodle.Parse.Attoparsec where

import           Control.Applicative 
import           Data.Attoparsec
import           Data.Attoparsec.Char8 ( char, decimal, double, skipSpace
                                      , isHorizontalSpace)
-- import qualified Data.Attoparsec.Iteratee as AI
import qualified Data.ByteString.Char8 as B hiding (map) 
import           Data.Char 
-- import qualified Data.Iteratee as Iter
-- import           Data.Iteratee.Char
import           Data.Strict.Tuple
-- from hoodle-platform 
import qualified Data.Hoodle.Simple as H
-- 
import Prelude hiding (takeWhile)

-- |
skipSpaces :: Parser () 
skipSpaces = satisfy isHorizontalSpace *> skipWhile isHorizontalSpace

-- | 
trim_starting_space :: Parser ()
trim_starting_space = do try endOfInput
                         <|> takeWhile (inClass " \n") *> return ()
                
-- | 
langle :: Parser Char 
langle = char '<'

-- | 
rangle :: Parser Char 
rangle = char '>'

-- | 
xmlheader :: Parser B.ByteString
xmlheader = string "<?" *> takeTill (inClass "?>") <* string "?>"
                 
-- | 
headercontentWorker :: B.ByteString -> Parser B.ByteString 
headercontentWorker  bstr = do 
  h <- takeWhile1 (notInClass "?>") 
  ((string "?>" >>= return . (bstr `B.append` h `B.append`))
   <|> headercontentWorker (bstr `B.append` h))

-- | 
headercontent :: Parser B.ByteString 
headercontent = headercontentWorker B.empty
                 
-- | 
stroketagopen :: Parser H.Stroke 
stroketagopen = do 
  trim 
  string "<stroke"
  trim 
  string "tool="
  char '"'
  tool <- alphabet 
  char '"'
  trim 
  string "color="
  char '"'
  color <- alphanumsharp 
  char '"'
  trim 
  string "width="
  char '"'
  width <- double 
  char '"'
  char '>' 
  return $ H.Stroke tool color width []   

  
-- | 
stroketagclose :: Parser B.ByteString 
stroketagclose = string "</stroke>"

-- | 
onestroke :: Parser H.Item 
onestroke =  do trim
                strokeinit <- stroketagopen
                coordlist <- many $ do trim_starting_space
                                       x <- double
                                       skipSpace 
                                       y <- double
                                       skipSpace 
                                       return (x :!: y)  
                stroketagclose 
                (return. H.ItemStroke) $ strokeinit { H.stroke_data=coordlist } 

-- | 
img :: Parser H.Item 
img = do trim 
         string "<img"
         trim 
         string "src=\""
         fsrc <- parseFileName 
         char '"'
         trim 
         string "x=\""
         posx <- double 
         char '"'
         trim
         string "y=\""
         posy <- double
         char '"'
         trim 
         string "width=\""
         width <- double
         char '"'
         trim 
         string "height=\""
         height <- double
         char '"'
         trim 
         string "/>"
         (return . H.ItemImage) (H.Image fsrc (posx,posy) (H.Dim width height))
         
         

-- | 
trim :: Parser ()
trim = trim_starting_space

                  
-- | 
hoodle :: Parser H.Hoodle 
hoodle  = do trim
             xmlheader <?> "xmlheader"
             trim
             hoodleheader <?> "hoodleheader"
             trim
             t <- title <?> "title"
             trim
             (try (preview >> return ())
              <|> return ()) 
             pgs <- many1 (page <?> "page")
             trim
             hoodleclose 
             return $ H.Hoodle t pgs 
             
page :: Parser H.Page 
page = do trim 
          dim <- pageheader
          trim 
          bkg <- background <?> "background"
          trim 
          layers <- many1 layer
          trim
          pageclose 
          return $ H.Page dim bkg layers
         
          
layer :: Parser H.Layer
layer = do trim
           layerheader
           trim
           -- s1 <- onestroke 
           -- s2 <- img
           -- let strokes = [s1,s2]
           itms <- many (try onestroke <|> img)
           trim
           layerclose 
           return $ H.Layer itms


title :: Parser B.ByteString 
title = do trim 
           titleheader
           str <- takeTill (inClass "<") -- (many . satisfy . notInClass ) "<"
           titleclose
           return str 

titleheader :: Parser B.ByteString          
titleheader = string "<title>"

titleclose :: Parser B.ByteString
titleclose = string "</title>"

preview :: Parser ()
preview = do trim 
             previewheader
             _str <- takeTill (inClass "<") 
             previewclose
             trim

previewheader :: Parser B.ByteString 
previewheader = string "<preview>"

previewclose :: Parser B.ByteString 
previewclose = string "</preview>"

hoodleheader :: Parser B.ByteString
hoodleheader = hoodleheaderstart *> takeTill (inClass ">") <* hoodleheaderend

hoodleheaderstart :: Parser B.ByteString 
hoodleheaderstart = string "<hoodle"

hoodleheaderend :: Parser Char
hoodleheaderend = char '>'

hoodleclose :: Parser B.ByteString
hoodleclose =  string "</hoodle>"

pageheader :: Parser H.Dimension 
pageheader = do pageheaderstart  
                trim
                string "width=" 
                char '"'
                w <- double
                char '"'
                trim 
                string "height="
                char '"' 
                h <- double 
                char '"'
                takeTill (inClass ">")
                pageheaderend
                return $ H.Dim w h
                 
pageheaderstart :: Parser B.ByteString
pageheaderstart = string "<page"

pageheaderend :: Parser Char
pageheaderend = char '>'

pageclose :: Parser B.ByteString                  
pageclose = string "</page>"

layerheader :: Parser B.ByteString
layerheader = string "<layer>"

layerclose :: Parser B.ByteString
layerclose = string "</layer>"

background :: Parser H.Background 
background = do 
    trim
    backgroundheader
    trim 
    string "type=" 
    char '"'
    typ <- alphabet
    char '"'
    case typ of 
      "solid" -> do 
        trim 
        string "color="
        char '"' 
        col <- alphanumsharp 
        char '"'
        trim 
        string "style="
        trim 
        char '"'
        sty <- alphabet 
        char '"' 
        trim 
        takeTill (inClass "/>") 
        backgroundclose
        return $ H.Background typ col sty 
      "pdf" -> do     
        trim <?> "trim0"
        (mdomain,mfilename) <- (try $ do  
                                 string "domain="
                                 char '"' 
                                 domain <- alphabet 
                                 char '"'
                                 trim <?> "trim1"
                                 string "filename="
                                 trim <?> "trim2"
                                 char '"'
                                 filename <- parseFileName <?> "filename parse"
                                 char '"' 
                                 return (Just domain, Just filename))
                               <|> return (Nothing,Nothing)
        trim <?> "trim3"
        string "pageno="
        trim <?> "trim4"
        char '"' 
        pnum <- decimal <?> "decimal"
        char '"'
        trim 
        takeTill (inClass "/>")  <?> "here takeTill"
        backgroundclose
        return $ H.BackgroundPdf typ mdomain mfilename pnum 
      _ -> fail "in parsing background"  
        
        
alphabet :: Parser B.ByteString
alphabet = takeWhile1 (\w -> (w >= 65 && w <= 90) || (w >= 97 && w <= 122)) 

alphanumsharp :: Parser B.ByteString            
alphanumsharp = takeWhile1 (\w -> (w >= 65 && w <= 90) 
                                  || (w >= 97 && w <= 122) 
                                  || ( w >= 48 && w<= 57 ) 
                                  || ( w== 35) ) 

-- | need to be reimplemented
parseFileName :: Parser B.ByteString
parseFileName = takeTill (inClass ['"'])
                -- takeWhilw1 (\w -> (w >= 65 && w <= 90) 
                --                   || (w >= 97 && w <= 122)
                --                   || (w >= 48 && w <= 57)
                --                   || (w == 35) 


-- |
backgroundheader :: Parser B.ByteString
backgroundheader = string "<background"

-- | 
backgroundclose :: Parser B.ByteString
backgroundclose = string "/>"

{-
iter_hoodle :: Iter.Iteratee B.ByteString IO Hoodle
iter_hoodle = AI.parserToIteratee parser_hoodle 

read_hoodle :: String -> IO Hoodle 
read_hoodle str = Iter.fileDriver iter_hoodle str 

read_xojgz :: String -> IO Hoodle 
read_xojgz str =  Iter.fileDriver (Iter.joinIM (ungzipXoj iter_hoodle)) str


cat_hoodlegz :: String -> IO () 
cat_hoodlegz str = Iter.fileDriver 
                      (Iter.joinIM (ungzipXoj printLinesUnterminated)) str 

onlyresult (Done _ r) = r 
-}

