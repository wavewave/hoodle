{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, 
             FlexibleInstances, DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Text.Xournal.Parse.Enumerator 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Text.Xournal.Parse.Enumerator where

import Debug.Trace
import qualified Data.ByteString as S
import Data.Enumerator as E hiding (foldl')
import qualified Data.Enumerator.List as EL 
import Control.Applicative 
import Control.Monad.Trans
import Control.Monad
import qualified Data.Text as T (dropWhile)
import Data.Text hiding (foldl', zipWith)
import Data.Text.Encoding
import Data.Text.Read
import Data.Strict.Tuple (  Pair(..) )
import Data.List (foldl') 
import Control.Category
import Data.Label

import Data.XML.Types
import Text.XML.Stream.Render 
import Text.XML.Stream.Parse hiding (many)
import System.IO 

import Data.Xournal.Simple 
import Data.Enumerator.Binary (enumHandle)
import Prelude hiding ((.),id)


-- * utils 

-- | 

lookAhead :: Monad m => Iteratee a m (Maybe a)
lookAhead = continue loop where
  loop (Chunks []) = lookAhead
  loop (Chunks (x:xs)) = yield (Just x) (Chunks (x:xs))
  loop EOF = yield Nothing EOF

-- |             

trc :: (Show a) => String -> a -> b -> b
trc str a b = trace (str ++ ":" ++ show a) b 
                     
-- |

flipap :: a -> (a -> b) -> b
flipap = flip ($)

-- | 

unit :: (Monad m) => m () 
unit = return ()

-- | 
 
skipspace :: Text -> Text 
skipspace = T.dropWhile (\c->(c==' ') || (c=='\n') || (c=='\r'))       
    
-- | 

many0event :: Monad m => 
              (Text,Text) 
              -> (Event -> E.Iteratee Event m (Either String a))
              -> E.Iteratee Event m (Either String [a])
many0event (start,end) iter = many1eventWrkr (start,end) id iter 


-- | 

many1event :: Monad m => 
              (Text,Text) 
              -> (Event -> E.Iteratee Event m (Either String a))
              -> E.Iteratee Event m (Either String [a])
many1event (start,end) iter = do
  EL.dropWhile (not.isStart start)
  EL.head >>= 
    maybe (return (Left ("error in " ++ unpack start))) 
          (\ev -> do ex <- iter ev
                     case ex of       
                       Left err -> return (Left err) 
                       Right x -> 
                         let acc = (x:)
                         in many1eventWrkr (start,end) acc iter)
  
-- |    
  
many1eventWrkr :: Monad m => 
                  (Text,Text) 
                  -> ( [a] -> [a]  ) 
                  -> (Event -> E.Iteratee Event m (Either String a))
                  -> E.Iteratee Event m (Either String [a])         
many1eventWrkr (start,end) acc iter = 
    drop2NextStartOrEnd >>= \e -> do 
      case e of 
        Left (txt,ev) -> 
          if txt == start 
          then do EL.drop 1 
                  ex <- iter ev
                  case ex of 
                    Left err -> return (Left err)
                    Right x -> 
                      many1eventWrkr (start,end) (acc.(x:)) iter  
          else return (Left ("got " ++ unpack txt))
        Right txt -> 
          if txt == end 
          then do return (Right (acc [])) 
          else return (Left ("got " ++ unpack txt))
  
  
-- | 

drop2NextStartOrEnd :: (Monad m) => 
                       Iteratee Event m (Either (Text,Event) Text)
drop2NextStartOrEnd = do 
  EL.dropWhile (not.isEventStartEnd)
  melm <- lookAhead 
  case melm of  
    Just elm@(EventBeginElement name _) 
      -> return (Left (nameLocalName name,elm))
    Just (EventEndElement name) 
      -> return (Right (nameLocalName name))
    Just _ -> error "this is impossible in drop2NextStartOrEnd"
    Nothing -> error "no more item in drop2NextStartOrEnd" 


-- * parsers 

-- | parse whole xournal file 

pXournal :: Monad m => Iteratee Event m (Either String Xournal)
pXournal = do  
  EL.dropWhile (not.isStart "xournal")
  EL.head >>= maybe (
    return (Left "no xournal"))
    (const $ do 
       title <- pTitle 
       pages <- many1event ("page","xournal") pPage
       (return $ Xournal <$> title <*> pages ))  
 
-- | parse one page 

pPage :: Monad m => Event -> Iteratee Event m (Either String Page)
pPage ev = do let dim = getDimension ev 
              bkg <- pBkg
              layers <- many1event ("layer","page") pLayer 
              EL.dropWhile (not.isEnd "page")
              EL.drop 1 
              return (Page <$> dim <*> bkg <*> layers )  

-- | 
              
pTitle :: Monad m => Iteratee Event m (Either String S.ByteString) 
pTitle = do EL.dropWhile (not.isStart "title")
            EL.drop 1
            EL.head >>= 
              maybe (return (Left "not title"))
                    (\ev -> do let title = getContent ev 
                               EL.dropWhile (not.isEnd "title")
                               EL.drop 1 
                               return (encodeUtf8 <$> title) )

-- | 

pBkg :: Monad m => Iteratee Event m (Either String Background) 
pBkg = do EL.dropWhile (not.isStart "background")
          EL.head >>= 
            maybe (return (Left "not background"))
                  (\ev -> do let bkg = getBackground ev 
                             EL.dropWhile (not.isEnd "background")
                             EL.drop 1 
                             return bkg) 

-- | 
          
pLayer :: Monad m => Event -> Iteratee Event m (Either String Layer) 
pLayer _ev = do strokes <- many0event ("stroke","layer") pStroke 
                EL.dropWhile (not.isEnd "layer")
                EL.drop 1 
                return (Layer <$> strokes)

-- | 
               
pStroke :: Monad m => Event -> Iteratee Event m (Either String Stroke) 
pStroke ev = do 
  let estr1wdth = getStroke ev 
  EL.head >>= 
    maybe (return (Left "pStroke ecoord"))
          (\elm -> do 
              let txt = getContent elm :: Either String Text
                  ctnt = getStrokeContent id =<< txt 
              EL.dropWhile (not.isEnd "stroke") 
              EL.drop 1 
              let f23 (x :!: y) z = (x,y,z)
              let rfunc d' (Stroke t c _ _, sw) = case sw of
                      SingleWidth w' -> Stroke t c w' d'  
                      VarWidth ws -> VWStroke t c  (zipWith f23 d' ws)
                  rfunc _ (VWStroke _ _ _ ,_) = 
                    error "this should not happen in pStroke"
              return $ rfunc <$> ctnt <*> estr1wdth)

-- * for each event 

-- |             

getStrokeContent :: ([Pair Double Double] -> [Pair Double Double])
                    -> Text
                    -> Either String [Pair Double Double]
getStrokeContent acc txt = 
  let eaction = do (x,rest1) <- double (skipspace txt)
                   (y,rest2) <- double (skipspace rest1)      
                   return (x :!: y, rest2)  
  in case eaction of 
       Left _str -> return (acc []) 
       Right (pxy,rest2) -> getStrokeContent (acc . (pxy:)) rest2

-- | 
 
getStroke :: Event -> Either String (Stroke,StrokeWidth) 
getStroke (EventBeginElement _name namecontent) = 
    foldl' f (Right (Stroke "" "" 0 [],SingleWidth 0)) namecontent 
  where  
    f acc@(Left _) _ = acc 
    f acc@(Right (str@(Stroke _t _c _w _d),wdth)) (name,contents) =            
      if nameLocalName name == "tool" 
      then let ContentText txt = Prelude.head contents 
           in Right (flip (set s_tool) str . encodeUtf8 $ txt, wdth)
      else if nameLocalName name == "color"           
      then let ContentText txt = Prelude.head contents 
           in Right (flip (set s_color) str . encodeUtf8 $ txt, wdth)
      else if nameLocalName name == "width" 
      then let ContentText txt = Prelude.head contents 
           in (,) str <$> getWidth id txt
      else acc 
    f (Right (VWStroke _ _ _,_)) (_,_) = error "this should not happen in getStroke"
getStroke _ = Left "not a stroke"

--    (str { stroke_tool = encodeUtf8 txt})

-- | 

data StrokeWidth = SingleWidth Double | VarWidth [Double]
                 deriving Show
  
-- |

getWidth :: ([Double] -> [Double])
            -> Text 
            -> Either String StrokeWidth
getWidth acc txt = 
    case double (skipspace txt) of 
       Left _str -> case acc [] of 
                      [] -> Left "no width in stroke"
                      w:[] -> Right (SingleWidth w)
                      ws -> Right (VarWidth ws)
       Right (x,rest1) -> getWidth (acc.(x:)) rest1  


-- | 
       
getBackground :: Event -> Either String Background
getBackground (EventBeginElement _name namecontent) = 
    foldl' f (Right (Background "" "" "")) namecontent 
  where  
    toBkgPdf (Background _t _c _s) = BackgroundPdf "pdf" Nothing Nothing 0
    toBkgPdf bkg@(BackgroundPdf _t _d _f _p) = bkg 
    toBkgNoPdf _t bkg@(Background _ _ _) = bkg
    toBkgNoPdf t (BackgroundPdf _t _d _f _p) = Background t "" "" 
    
    f acc@(Left _) _ = acc 
    f acc@(Right bkg@(Background t c s)) (name,contents) =            
      if nameLocalName name == "type" 
      then let ContentText txt = Prelude.head contents 
           in if txt == "pdf" 
              then Right (toBkgPdf bkg)
              else Right (toBkgNoPdf (encodeUtf8 txt) bkg)
      else if nameLocalName name == "color"           
      then let ContentText txt = Prelude.head contents 
           in Right . (\x -> Background t x s) . encodeUtf8 $ txt 
      else if nameLocalName name == "style" 
      then let ContentText txt = Prelude.head contents 
           in Right . (\x -> Background t c x) . encodeUtf8 $ txt 
      else acc 
    f acc@(Right bkg@(BackgroundPdf t d f p)) (name,contents) =            
      if nameLocalName name == "type" 
      then let ContentText txt = Prelude.head contents 
           in if txt == "pdf" 
              then Right (toBkgPdf bkg)
              else Right (toBkgNoPdf (encodeUtf8 txt) bkg)
      else if nameLocalName name == "domain"           
      then let ContentText txt = Prelude.head contents 
           in Right . (\x -> BackgroundPdf t x f p) . Just . encodeUtf8 $ txt 
      else if nameLocalName name == "filename" 
      then let ContentText txt = Prelude.head contents 
           in Right . (\x -> BackgroundPdf t d x p) . Just . encodeUtf8 $ txt 
      else if nameLocalName name == "pageno" 
      then let ContentText txt = Prelude.head contents 
           in (\x -> BackgroundPdf t d f x) . fst <$> decimal txt 
      else acc 
getBackground _ = Left "not a background"
           
-- | 

getDimension :: Event -> Either String Dimension            
getDimension (EventBeginElement _name namecontent) = 
    foldl' f (Right (Dim 0 0)) namecontent 
  where 
    f acc@(Left _) _ = acc 
    f acc@(Right (Dim w h)) (nm,contents) =            
      if nameLocalName nm == "width" 
      then let ContentText txt = Prelude.head contents 
           in (flip Dim h) . fst <$> double txt 
      else if nameLocalName nm == "height"           
      then let ContentText txt = Prelude.head contents 
           in (Dim w) . fst <$> double txt
      else acc 
getDimension _ = Left "not a dimension"

-- | get Content   

getContent :: Event -> Either String Text
getContent (EventContent (ContentText txt)) = Right txt 
getContent _ = Left "no content"
  

-- * predicates 

-- | 

isEventStartEnd :: Event -> Bool 
isEventStartEnd (EventBeginElement _ _) = True
isEventStartEnd (EventEndElement _ ) = True
isEventStartEnd _ = False 

-- | check start of element with name txt  
  
isStart :: Text -> Event -> Bool 
isStart txt (EventBeginElement name _) = nameLocalName name == txt
isStart _ _ = False 

-- | check end of element with name txt 

isEnd :: Text -> Event -> Bool
isEnd txt (EventEndElement name) = nameLocalName name == txt 
isEnd _ _ = False 


-- * driver routines 

-- | generic xml file driver

parseXmlFile :: (MonadIO m) => Handle -> E.Iteratee Event m a -> m a
parseXmlFile h iter = do 
  run_ $ enumHandle 4096 h $$ joinI $ parseBytes def $$ iter 


-- | for xournal 

parseXojFile :: FilePath -> IO (Either String Xournal)
parseXojFile fp = withFile fp ReadMode $ \ih -> parseXmlFile ih pXournal     

-- | printing for debug

iterPrint :: (Show s,MonadIO m) => E.Iteratee s m () 
iterPrint = do
  x <- EL.head
  maybe (return ()) (liftIO . print >=> \_ -> iterPrint) x


{-
-- |  

parseXmlFile :: (MonadIO m) => Handle -> E.Iteratee Event m a -> m a
parseXmlFile h iter = do 
  run_ $ enumHandle 4096 h $$ joinI $ parseBytes def $$ iter 
-}


-- Test functions
-- This is new xml event approach of xournal parsing



{-
isPageStart :: Event -> Bool 
isPageStart (EventBeginElement name _) = nameLocalName name == "page"
isPageStart _ = False 

isPageEnd :: Event -> Bool 
isPageEnd (EventEndElement name ) = nameLocalName name == "page"
isPageEnd _ = False 
-}

{-    dropWhileNConsume (not.isStart "xournal") 
    *> pTitle 
    <* dropWhileNConsume (not.isEnd "xournal") -}
{-    Nothing -> return ()
    Just y -> liftIO (print y) >> iterPrint -}
{-         
parseUpToFirstPage :: (Monad m) => E.Iteratee Event m [Event] 
parseUpToFirstPage = do 
  evs <- EL.takeWhile (not.isPageStart)
  return evs 
-}  
{-  
  dropWhileNConsume (not.isStart "title")
         *> (fmap getContent . maybe (Left "no title") Right <$> EL.head)
         <* dropWhileNConsume (not.isEnd "title") -}

{-               
parsePages :: (Monad m) => E.Iteratee Event m Page
parsePages = E.sequence $ do 
                 EL.dropWhile (not.isPageStart)
                 EL.drop 1
                 bkg <- parseBackground
                 ls <- parseLayers  
                 EL.takeWhile (not.isPageStart)
                 EL.drop 1 
                 return (Page ev 
            
myTakeWhile :: (s -> Bool) -> Step s m a -> Step s m a 
myTakeWhile chk iter = 
  mel <- EL.head 
  case mel of 
    Nothing -> return 
    Just el -> 
-}               

-- consumeStrokes x = chunkAsStrokes =$ x
-- | title 

{-
pTitle :: Monad m => Iteratee Event m (Either String Text)
pTitle = withEvent "title" $ do
           melm <- EL.head 
           case melm of 
             Nothing -> return (Left "title" )
             Just elm -> return (getContent elm)
-}         


-- | 
{-  
instance (Monad m) => Alternative (E.Iteratee Event m) where
  empty = mzero 
  a1 <|> a2 =  
-}

{-
instance (MonadPlus m) => MonadPlus (Iteratee s m) where
  mzero = lift mzero 
  a `mplus` b = E.Iteratee (E.runIteratee a `mplus` E.runIteratee b)
-}

-- | 
{-   

iterMany :: (MonadPlus m) => Iteratee s m a -> Iteratee s m [a] 
iterMany = unwrapMonad . many . WrapMonad 
-}  

{-
onePrint :: (Monad m, Show s) => Iteratee s m () 
onePrint = do 
  EL.head >>=
    maybe (return ()) 
          (\x -> trace ("dropped item" ++ show x) (return ()))
-- | 
  
dropWhileNConsume :: (Monad m, Show s) => (s -> Bool) -> Iteratee s m () 
dropWhileNConsume f = do
    str <- EL.takeWhile f 
    trace ("dropWhileNConsume" ++ show str) unit
    onePrint -- EL.drop 1

-- | 

upToEventStart :: (Monad m) => Text -> Iteratee Event m () 
upToEventStart txt = dropWhileNConsume (not.isStart txt)

-- | 

upToEventEnd :: (Monad m) => Text -> Iteratee Event m () 
upToEventEnd txt = dropWhileNConsume (not.isEnd txt)
                                     
-- | 

withEvent :: Monad m => Text -> Iteratee Event m a -> Iteratee Event m a 
withEvent txt iter = upToEventStart txt *> iter <* upToEventEnd txt 

-- data SuccessOrFail a b = Success a | Fail Event  
-}

