{-# LANGUAGE OverloadedStrings #-}

module Text.Xournal.Parse where

import Control.Applicative hiding (many)

import Debug.Trace

import qualified Data.Attoparsec as P 
import Data.Attoparsec
import Data.Attoparsec.Char8 (char, double, anyChar, skipSpace, isHorizontalSpace, notChar, isAlpha_ascii)
import qualified Data.ByteString.Char8 as B hiding (map) 
import Data.ByteString.Internal (c2w,w2c)
import Data.ByteString.Lex.Double

-- import Data.Attoparsec.Incremental.Char8

import qualified Data.Iteratee as Iter
import qualified Data.ListLike as LL
import Data.Iteratee.Util
import Data.Iteratee.Char

import qualified Data.Attoparsec.Iteratee as AI
import Data.Word (Word8)
import Data.Char 

import Control.Monad.IO.Class

import Text.Xournal.Type
import Text.Xournal.Parse.Zlib

import Data.Strict.Tuple

import Prelude hiding (takeWhile)

skipSpaces :: Parser () 
skipSpaces = satisfy isHorizontalSpace *> skipWhile isHorizontalSpace

trim_starting_space :: Parser ()
trim_starting_space = do try endOfInput
                         <|> takeWhile (inClass " \n") *> return ()


--                         <|> (many . satisfy . inClass ) " \n" *> return () 
                
langle :: Parser Char 
langle = char '<'

rangle :: Parser Char 
rangle = char '>'

xmlheader :: Parser B.ByteString
xmlheader = string "<?" *> takeTill (inClass "?>") <* string "?>"
                 
headercontentWorker :: B.ByteString -> Parser B.ByteString 
headercontentWorker  bstr = do 
  h <- takeWhile1 (notInClass "?>") 
  ((string "?>" >>= return . (bstr `B.append` h `B.append`))
   <|> headercontentWorker (bstr `B.append` h))

headercontent :: Parser B.ByteString 
headercontent = headercontentWorker B.empty
                 
stroketagopen :: Parser Stroke --  B.ByteString 
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
  return $ Stroke tool color width []   

stroketagclose :: Parser B.ByteString 
stroketagclose = string "</stroke>"

onestroke :: Parser Stroke 
onestroke =  do trim
                strokeinit <- stroketagopen
                coordlist <- many $ do trim_starting_space
                                       x <- double
                                       skipSpace 
                                       y <- double
                                       skipSpace 
                                       return (x :!: y)  
                stroketagclose 
                return $ strokeinit { stroke_data = coordlist } 


trim = trim_starting_space

parser_xournal :: Parser Xournal
parser_xournal = do trim
                    xmlheader
                    trim
                    xournal
                  

xournal :: Parser Xournal 
xournal = do trim 
             xournalheader
             trim
             t <- title
             trim
             (try (preview >> return ())
              <|> return ()) 
             pgs <- many1 page
             trim
             xournalclose 
             return $ Xournal  t pgs 
             
page :: Parser Page 
page = do trim 
          dim <- pageheader
          trim 
          bkg <- background 
          trim 
          layers <- many1 layer
          trim
          pageclose 
          return $ Page dim bkg layers
         
          
layer :: Parser Layer
layer = do trim
           layerheader
           trim
           strokes <- many onestroke
           trim
           layerclose 
           return $ Layer strokes


title :: Parser B.ByteString 
title = do trim 
           titleheader
           str <- takeTill (inClass "<") -- (many . satisfy . notInClass ) "<"
           titleclose
           return str 
          
titleheader = string "<title>"
titleclose = string "</title>"

preview :: Parser ()
preview = do trim 
             previewheader
             str <- takeTill (inClass "<") 
             previewclose
             trim

previewheader = string "<preview>"
previewclose = string "</preview>"

xournalheader = xournalheaderstart *> takeTill (inClass ">") <* xournalheaderend
xournalheaderstart = string "<xournal"
xournalheaderend = char '>'
xournalclose =  string "</xournal>"

pageheader :: Parser Dimension 
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
                return $ Dim w h
                 
pageheaderstart = string "<page"
pageheaderend = char '>'
                  
pageclose = string "</page>"

layerheader = string "<layer>"

layerclose = string "</layer>"

background :: Parser Background 
background = do trim
                backgroundheader
                trim 
                string "type=" 
                char '"'
                typ <- alphabet
                char '"'
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
                takeTill (inClass "/>") -- ( many . satisfy . notInClass ) "/>"
                backgroundclose
                return $ Background typ col sty 
    

alphabet = takeWhile1 (\w -> (w >= 65 && w <= 90) || (w >= 97 && w <= 122)) 
            
alphanumsharp = takeWhile1 (\w -> (w >= 65 && w <= 90) 
                                  || (w >= 97 && w <= 122) 
                                  || ( w >= 48 && w<= 57 ) 
                                  || ( w== 35) ) 

backgroundheader = string "<background"
backgroundclose = string "/>"

iter_xournal :: Iter.Iteratee B.ByteString IO Xournal
iter_xournal = AI.parserToIteratee parser_xournal 

read_xournal :: String -> IO Xournal 
read_xournal str = Iter.fileDriver iter_xournal str 

read_xojgz :: String -> IO Xournal 
read_xojgz str =  Iter.fileDriver (Iter.joinIM (ungzipXoj iter_xournal)) str


cat_xournalgz :: String -> IO () 
cat_xournalgz str = Iter.fileDriver 
                      (Iter.joinIM (ungzipXoj printLinesUnterminated)) str 


-- printIter :: Iter.Iteratee B.ByteString IO () 
-- printIter = 



   
onlyresult (Done _ r) = r 

