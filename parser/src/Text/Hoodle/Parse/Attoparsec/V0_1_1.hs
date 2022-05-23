{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Text.Hoodle.Parse.Attoparsec.V0_1_1 where

import Control.Applicative
import Control.Monad (void)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
  ( anyChar,
    char,
    decimal,
    double,
    isHorizontalSpace,
    skipSpace,
  )
import qualified Data.ByteString.Char8 as B hiding (map)
import Data.Char
import Data.Functor (($>), (<&>))
import qualified Data.Hoodle.Simple.V0_1_1 as H
import Data.Strict.Tuple
import Prelude hiding (takeWhile)

-- |
skipSpaces :: Parser ()
skipSpaces = satisfy isHorizontalSpace *> skipWhile isHorizontalSpace

-- |
trimStartingSpace :: Parser ()
trimStartingSpace =
  do try endOfInput
    <|> (takeWhile (inClass " \n") $> ())

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
headercontentWorker bstr = do
  h <- takeWhile1 (notInClass "?>")
  (string "?>" <&> (bstr `B.append` h `B.append`))
    <|> headercontentWorker (bstr `B.append` h)

-- |
headercontent :: Parser B.ByteString
headercontent = headercontentWorker B.empty

-- |
data StrokeWidth = SingleWidth Double | VarWidth [Double]

-- |
data XmlStroke = XmlStroke
  { xstrk_tool :: B.ByteString,
    xstrk_color :: B.ByteString,
    xstrk_width :: StrokeWidth,
    xstrk_xydata :: [Pair Double Double]
  }

-- |
xmlstroketagopen :: Parser XmlStroke
xmlstroketagopen = do
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
  -- width <- double
  width <- strokewidth
  char '>'
  return $ XmlStroke tool color width []

strokewidth :: Parser StrokeWidth
strokewidth = do
  char '"'
  wlst <- many $ do
    trimStartingSpace
    w <- double
    skipSpace
    return w
  char '"'
  let msw
        | length wlst == 1 = return (SingleWidth (head wlst))
        | null wlst = fail "no width"
        | otherwise = return (VarWidth wlst)
  msw

-- |
xmlstroketagclose :: Parser ()
xmlstroketagclose = void $ string "</stroke>"

-- |
xmlstroke :: Parser XmlStroke
xmlstroke = do
  trim
  strokeinit <- xmlstroketagopen
  coordlist <- many $ do
    trimStartingSpace
    x <- double
    skipSpace
    y <- double
    skipSpace
    return (x :!: y)
  xmlstroketagclose
  return $ strokeinit {xstrk_xydata = coordlist}

-- |
onestroke :: Parser H.Item
onestroke = do
  xstrk <- xmlstroke
  let r = case xstrk_width xstrk of
        SingleWidth w ->
          ( H.Stroke <$> xstrk_tool
              <*> xstrk_color
              <*> pure w
              <*> xstrk_xydata
          )
            xstrk
        VarWidth ws ->
          let xyz = mkXYZ (xstrk_xydata xstrk) ws
           in ( H.VWStroke <$> xstrk_tool <*> xstrk_color
                  <*> pure xyz
              )
                xstrk
  (return . H.ItemStroke) r

-- |
mkXYZ :: [Pair Double Double] -> [Double] -> [(Double, Double, Double)]
mkXYZ = zipWith f where f (x :!: y) z = (x, y, z)

-- |
img :: Parser H.Item
img = do
  trim
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
  (return . H.ItemImage) (H.Image fsrc (posx, posy) (H.Dim width height))

svgHeader :: Parser ((Double, Double), H.Dimension)
svgHeader = do
  trim
  string "<svgobject"
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
  string ">"
  return ((posx, posy), H.Dim width height)

svgFooter :: Parser ()
svgFooter = void $ string "</svgobject>"

svgText :: Parser B.ByteString
svgText = do
  string "<text>"
  str <- string "<![CDATA[" *> manyTill anyChar (try (string "]]>"))
  string "</text>"
  return (B.pack str)

svgCommand :: Parser B.ByteString
svgCommand = do
  string "<command>"
  str <- string "<![CDATA[" *> manyTill anyChar (try (string "]]>"))
  string "</command>"
  return (B.pack str)

svgRender :: Parser B.ByteString
svgRender = do
  string "<render>"
  str <- string "<![CDATA[" *> manyTill anyChar (try (string "]]>"))
  string "</render>"
  return (B.pack str)

svgObj :: Parser H.Item
svgObj = do
  (xy, dim) <- svgHeader
  trim
  (mt, mc) <-
    try
      ( do
          t <- svgText
          trim
          c <- svgCommand
          return (Just t, Just c)
      )
      <|> try (svgText >>= \t -> return (Just t, Nothing))
      <|> return (Nothing, Nothing)
  trim
  bstr <- svgRender
  trim
  svgFooter
  (return . H.ItemSVG) (H.SVG mt mc bstr xy dim)

-- |
trim :: Parser ()
trim = trimStartingSpace

-- |
hoodle :: Parser H.Hoodle
hoodle = do
  trim
  xmlheader <?> "xmlheader"
  trim
  hoodleheader <?> "hoodleheader"
  trim
  t <- title <?> "title"
  trim
  try (void preview)
    <|> return ()
  pgs <- many1 (page <?> "page")
  trim
  hoodleclose
  return $ H.Hoodle t pgs

page :: Parser H.Page
page = do
  trim
  dim <- pageheader
  trim
  bkg <- background <?> "background"
  trim
  layers <- many1 layer
  trim
  pageclose
  return $ H.Page dim bkg layers

layer :: Parser H.Layer
layer = do
  trim
  layerheader <?> "layer"
  trim
  itms <- many (try onestroke <|> try img <|> svgObj)
  trim
  layerclose
  return $ H.Layer itms

title :: Parser B.ByteString
title = do
  trim
  titleheader
  str <- takeTill (inClass "<")
  titleclose
  return str

titleheader :: Parser B.ByteString
titleheader = string "<title>"

titleclose :: Parser B.ByteString
titleclose = string "</title>"

preview :: Parser ()
preview = do
  trim
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
hoodleclose = string "</hoodle>"

pageheader :: Parser H.Dimension
pageheader = do
  pageheaderstart
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
      (mdomain, mfilename) <-
        try
          ( do
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
              return (Just domain, Just filename)
          )
          <|> return (Nothing, Nothing)
      trim <?> "trim3"
      string "pageno="
      trim <?> "trim4"
      char '"'
      pnum <- decimal <?> "decimal"
      char '"'
      trim
      takeTill (inClass "/>") <?> "here takeTill"
      backgroundclose
      return $ H.BackgroundPdf typ mdomain mfilename pnum
    _ -> fail "in parsing background"

alphabet :: Parser B.ByteString
alphabet = takeWhile1 (\w -> (w >= 65 && w <= 90) || (w >= 97 && w <= 122))

alphanumsharp :: Parser B.ByteString
alphanumsharp =
  takeWhile1
    ( \w ->
        (w >= 65 && w <= 90)
          || (w >= 97 && w <= 122)
          || (w >= 48 && w <= 57)
          || (w == 35)
    )

-- | need to be reimplemented
parseFileName :: Parser B.ByteString
parseFileName = takeTill (inClass ['"'])

-- |
backgroundheader :: Parser B.ByteString
backgroundheader = string "<background"

-- |
backgroundclose :: Parser B.ByteString
backgroundclose = string "/>"
