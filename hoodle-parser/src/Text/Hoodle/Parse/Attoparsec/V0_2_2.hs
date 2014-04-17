{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Text.Hoodle.Parse.Attoparsec.V0_2_2
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- attoparsec implementation of hoodle parser v0.2.2
-- 
-----------------------------------------------------------------------------

module Text.Hoodle.Parse.Attoparsec.V0_2_2 where

import           Control.Applicative 
import           Data.Attoparsec
import           Data.Attoparsec.Char8 ( char, decimal, double, skipSpace
                                      , isHorizontalSpace, anyChar)
import qualified Data.ByteString.Char8 as B hiding (map) 
import           Data.Char 
import           Data.Strict.Tuple
-- from hoodle-platform 
import qualified Data.Hoodle.Simple.V0_2_2 as H
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
data StrokeWidth = SingleWidth Double | VarWidth [Double] 


-- | 
data XmlStroke = XmlStroke { xstrk_tool :: B.ByteString  
                           , xstrk_color :: B.ByteString
                           , xstrk_width :: StrokeWidth
                           , xstrk_xydata :: [Pair Double Double] }

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
    wlst <- many $ do trim_starting_space
                      w <- double
                      skipSpace 
                      return w
    char '"'
    let msw | length wlst == 1 = return (SingleWidth (head wlst)) 
            | length wlst == 0 = fail "no width"
            | otherwise = return (VarWidth wlst)
    msw

  
-- | 
xmlstroketagclose :: Parser ()
xmlstroketagclose = string "</stroke>" >> return ()


-- | 
xmlstroke :: Parser XmlStroke                 
xmlstroke = do 
    trim 
    strokeinit <- xmlstroketagopen
    coordlist <- many $ do trim_starting_space
                           x <- double
                           skipSpace 
                           y <- double
                           skipSpace 
                           return (x :!: y)  
    xmlstroketagclose 
    return $ strokeinit { xstrk_xydata = coordlist } 


-- | 
onestroke :: Parser H.Stroke 
onestroke =  do 
    xstrk <- xmlstroke 
    let r = case xstrk_width xstrk of 
              SingleWidth w -> (H.Stroke <$> xstrk_tool 
                                 <*> xstrk_color 
                                 <*> pure w 
                                 <*> xstrk_xydata ) xstrk
              VarWidth ws -> let xyz = mkXYZ (xstrk_xydata xstrk) ws
                             in (H.VWStroke <$> xstrk_tool <*> xstrk_color   
                                  <*> pure xyz) xstrk
    return r 

-- | 
mkXYZ :: [Pair Double Double] -> [Double] -> [(Double,Double,Double)]
mkXYZ = zipWith f where f (x :!: y) z = (x,y,z)

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
         

svg_header :: Parser ((Double,Double),H.Dimension)
svg_header = do trim 
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
                return ((posx,posy),H.Dim width height) 

svg_footer :: Parser () 
svg_footer = string "</svgobject>" >> return ()

textCDATA :: Parser B.ByteString 
textCDATA = do 
    string "<text>"
    str <- string "<![CDATA[" *> manyTill anyChar (try (string "]]>"))
    string "</text>"
    return (B.pack str) 

commandCDATA :: Parser B.ByteString 
commandCDATA = do 
    string "<command>"
    str <- string "<![CDATA[" *> manyTill anyChar (try (string "]]>"))
    string "</command>"
    return (B.pack str)

renderCDATA :: Parser B.ByteString 
renderCDATA = do 
  string "<render>"
  str <- string "<![CDATA[" *> manyTill anyChar (try (string "]]>"))
  string "</render>"
  return (B.pack str)


svg_obj :: Parser H.Item 
svg_obj = do (xy,dim) <- svg_header
             trim 
             (mt,mc) <- (try (do t <- textCDATA 
                                 trim 
                                 c <- commandCDATA 
                                 return (Just t, Just c)) 
                         <|> try (textCDATA >>= \t -> return (Just t, Nothing))
                         <|> return (Nothing,Nothing))
             trim 
             bstr <- renderCDATA 
             trim 
             svg_footer
             (return . H.ItemSVG) (H.SVG mt mc bstr xy dim)

                                  

link_header :: Parser (B.ByteString,B.ByteString,Maybe B.ByteString,B.ByteString,(Double,Double),H.Dimension)
link_header = do 
    trim 
    string "<link"
    trim 
    i <- B.pack <$> (string "id=\"" *> manyTill anyChar (try (char '"')))
    trim 
    typ <- B.pack <$> (string "type=\"" *> manyTill anyChar (try (char '"')))
    trim 
    mlid <- case typ of 
      "simple" -> return Nothing 
      "linkdocid" -> Just<$>(string "linkedid=\"" *> takeTill (inClass "\"")<* char '"')
      _ -> fail "unknown link type"
    trim    
    loc <- B.pack <$> (string "location=\"" *> manyTill anyChar (try (char '"')))
    trim 
    posx <- string "x=\"" *> double <* char '"'
    trim
    posy <- string "y=\"" *> double <* char '"'
    trim 
    width <- string "width=\"" *> double <* char '"'
    trim 
    height <- string "height=\"" *> double <* char '"'
    trim 
    string ">"
    return (i,typ,mlid,loc,(posx,posy),H.Dim width height) 

link_footer :: Parser () 
link_footer = string "</link>" >> return ()


link :: Parser H.Item 
link = do 
    (i,typ,mlid,loc,xy,dim) <- link_header
    trim 
    (mt,mc) <- (try (do t <- textCDATA 
                        trim 
                        c <- commandCDATA 
                        return (Just t, Just c)) 
                <|> try (textCDATA >>= \t -> return (Just t, Nothing))
                <|> return (Nothing,Nothing))
    trim 
    bstr <- renderCDATA 
    trim 
    link_footer
    return . H.ItemLink $ 
      flip ($) mlid $ maybe (H.Link i typ loc mt mc bstr xy dim)
                            (\lid -> H.LinkDocID i lid loc mt mc bstr xy dim) 




-- | 
trim :: Parser ()
trim = trim_starting_space

-- | 
checkHoodleVersion :: Parser B.ByteString
checkHoodleVersion = do trim 
                        xmlheader
                        trim 
                        hoodleheaderstart
                        trim 
                        hoodleversion 

                  
-- | 
hoodle :: Parser H.Hoodle 
hoodle  = do trim
             xmlheader <?> "xmlheader"
             trim
             (_v,hid) <- hoodleheader <?> "hoodleheader"
             trim
             t <- title <?> "title"
             trim
             revs <- many (revision <?> "revision")
             skipSpace
             pdf <- (try (Just <$> embeddedpdf)
                     <|> return Nothing )
             pgs <- many1 (page <?> "page")
             trim
             hoodleclose 
             return $ H.Hoodle hid t revs pdf pgs 
             
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
           layerheader <?> "layer"
           trim
           itms <- many (try (H.ItemStroke <$> onestroke) <|> try img <|> try svg_obj <|> link)
           trim
           layerclose 
           return $ H.Layer itms


title :: Parser B.ByteString 
title = do trim 
           titleheader
           str <- takeTill (inClass "<") 
           titleclose
           return str 

titleheader :: Parser B.ByteString          
titleheader = string "<title>"

titleclose :: Parser B.ByteString
titleclose = string "</title>"

revision :: Parser H.Revision 
revision = do skipSpace
              string "<revision" 
              skipSpace 
              string "revmd5=\""
              md5str <- manyTill anyChar (try (char '"'))
              skipSpace 
              (try (do string "revtxt=\""
                       txtstr <- manyTill anyChar (try (char '"')) 
                       skipSpace
                       string "/>"
                       return (H.Revision (B.pack md5str) (B.pack txtstr)))
               <|> (do string "type=\"ink\""
                       skipSpace
                       char '>'
                       skipSpace
                       strks <- many1 onestroke
                       skipSpace
                       string "</revision>"
                       return (H.RevisionInk (B.pack md5str) strks)))
              
                      


embeddedpdf :: Parser (B.ByteString) 
embeddedpdf = do string "<embeddedpdf" 
                 trim 
                 string "src=\""
                 str <- manyTill anyChar (try (char '"'))
                 trim 
                 string "/>" 
                 return (B.pack str)


hoodleheader :: Parser (B.ByteString,B.ByteString)
hoodleheader = do hoodleheaderstart 
                  trim 
                  v <- hoodleversion
                  trim 
                  hid <- hoodleid
                  trim 
                  char '>' 
                  return (v,hid)

hoodleheaderstart :: Parser B.ByteString 
hoodleheaderstart = string "<hoodle"


hoodleversion :: Parser B.ByteString 
hoodleversion = string "version=\"" *> takeTill (inClass "\"") <* char '"'

hoodleid :: Parser B.ByteString 
hoodleid = string "id=\"" *> takeTill (inClass "\"") <* char '"'

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
      "embedpdf" -> do     
        trim <?> "trim0"
        string "pageno="
        char '"'
        pnum <- decimal <?> "embedpdf decimal" 
        char '"'
        trim 
        backgroundclose
        return $ H.BackgroundEmbedPdf  typ pnum 
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


-- |
backgroundheader :: Parser B.ByteString
backgroundheader = string "<background"

-- | 
backgroundclose :: Parser B.ByteString
backgroundclose = string "/>"

