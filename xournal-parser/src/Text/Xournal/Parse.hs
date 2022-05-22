{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : Text.Xournal.Parse
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
module Text.Xournal.Parse (module Text.Xournal.Parse.Conduit) where

import Text.Xournal.Parse.Conduit

{-
-- from other packages by others
import           Control.Applicative
import           Data.Attoparsec
import           Data.Attoparsec.Char8 ( char, decimal, double, skipSpace
                                       , isHorizontalSpace)
import qualified Data.ByteString.Char8 as B hiding (map)
import           Data.Char
import qualified Data.Conduit as C
import           Data.Conduit.Attoparsec
import           Data.Strict.Tuple
-- from other hoodle packages
import           Data.Xournal.Simple
-- from this package
import           Text.Xournal.Parse.Zlib
--
import Prelude hiding (takeWhile)

-- import qualified Data.Iteratee as Iter
-- import Data.Iteratee.Char
-- import qualified Data.Attoparsec.Iteratee as AI
-}

{-
--  |

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

-- |

stroketagclose :: Parser B.ByteString
stroketagclose = string "</stroke>"

-- |

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

-- |

trim :: Parser ()
trim = trim_starting_space

-- |

parser_xournal :: Parser Xournal
parser_xournal = do trim
                    xmlheader <?> "xmlheader"
                    trim
                    xournal <?> "xournal"

-- |

xournal :: Parser Xournal
xournal = do trim
             xournalheader <?> "xournalheader"
             trim
             t <- title <?> "title"
             trim
             (try (preview >> return ())
              <|> return ())
             pgs <- many1 (page <?> "page")
             trim
             xournalclose
             return $ Xournal  t pgs

-- |

page :: Parser Page
page = do trim
          dim <- pageheader
          trim
          bkg <- background <?> "background"
          trim
          layers <- many1 layer
          trim
          pageclose
          return $ Page dim bkg layers

-- |

layer :: Parser Layer
layer = do trim
           layerheader
           trim
           strokes <- many onestroke
           trim
           layerclose
           return $ Layer strokes

-- |

title :: Parser B.ByteString
title = do trim
           titleheader
           str <- takeTill (inClass "<") -- (many . satisfy . notInClass ) "<"
           titleclose
           return str

-- |

titleheader :: Parser B.ByteString
titleheader = string "<title>"

-- |

titleclose :: Parser B.ByteString
titleclose = string "</title>"

-- |

preview :: Parser ()
preview = do trim
             previewheader
             str <- takeTill (inClass "<")
             previewclose
             trim

-- |

previewheader :: Parser B.ByteString
previewheader = string "<preview>"

-- |

previewclose :: Parser B.ByteString
previewclose = string "</preview>"

-- |

xournalheader :: Parser B.ByteString
xournalheader = xournalheaderstart *> takeTill (inClass ">") <* xournalheaderend

-- |

xournalheaderstart :: Parser B.ByteString
xournalheaderstart = string "<xournal"

-- |

xournalheaderend :: Parser Char
xournalheaderend = char '>'

-- |

xournalclose :: Parser B.ByteString
xournalclose =  string "</xournal>"

-- |

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

-- |

pageheaderstart :: Parser B.ByteString
pageheaderstart = string "<page"

-- |

pageheaderend :: Parser Char
pageheaderend = char '>'

-- |

pageclose :: Parser B.ByteString
pageclose = string "</page>"

-- |

layerheader :: Parser B.ByteString
layerheader = string "<layer>"

-- |

layerclose :: Parser B.ByteString
layerclose = string "</layer>"

-- |

background :: Parser Background
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
        takeTill (inClass "/>") -- ( many . satisfy . notInClass ) "/>"
        backgroundclose
        return $ Background typ col sty
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
        return $ BackgroundPdf typ mdomain mfilename pnum

-- |

alphabet :: Parser B.ByteString
alphabet = takeWhile1 (\w -> (w >= 65 && w <= 90) || (w >= 97 && w <= 122))

-- |

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

-- |

iter_xournal :: Sink B.ByteString IO Xournal
iter_xournal = AI.parserToIteratee parser_xournal

-- |

read_xournal :: String -> IO Xournal
read_xournal str = Iter.fileDriver iter_xournal str

-- |

read_xojgz :: String -> IO Xournal
read_xojgz str =  Iter.fileDriver (Iter.joinIM (ungzipXoj iter_xournal)) str

-- |

cat_xournalgz :: String -> IO ()
cat_xournalgz str = Iter.fileDriver
                      (Iter.joinIM (ungzipXoj printLinesUnterminated)) str

-- |

onlyresult (Done _ r) = r
-}
