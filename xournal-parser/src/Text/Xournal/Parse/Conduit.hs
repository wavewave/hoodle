{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, 
             FlexibleInstances, DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Text.Xournal.Parse.Conduit
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Text.Xournal.Parse.Conduit where

-- from other packages
import           Control.Applicative 
import           Control.Category
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans
import qualified Data.ByteString as S
import           Data.Conduit 
import           Data.Conduit.Binary hiding (dropWhile) 
import           Data.Conduit.List as CL
import           Data.Conduit.Zlib
-- import           Data.Label
import           Data.List (foldl') 
import           Data.Strict.Tuple (  Pair(..) )
import qualified Data.Text as T -- hiding (foldl', zipWith)
import           Data.Text.Encoding
import           Data.Text.Read
import           Data.XML.Types
import           Text.XML.Stream.Render 
import           Text.XML.Stream.Parse hiding (many)
import           System.IO 
-- from other hoodle related packages
import           Data.Xournal.Simple 
-- from this package
import           Text.Xournal.Parse.Zlib
-- 
import           Prelude hiding ((.),id,dropWhile)
 

-- import Data.Enumerator as E hiding (foldl')
-- import qualified Data.Enumerator.List as EL
-- import Data.Enumerator.Binary (enumHandle, enumFile)
-- import qualified Codec.Zlib.Enum as EZ

-- * utils 

-- |

dropWhile :: (Show a, Monad m) => (a -> Bool) -> Sink a m () 
dropWhile p = do 
  x <- peek 
  case x of 
    Nothing -> return ()
    Just e -> if p e 
              then CL.drop 1 >> dropWhile p 
              else return ()

-- | 
{-
lookAhead :: Monad m => Sink a m (Maybe a)
lookAhead = continue loop where
  loop (Chunks []) = lookAhead
  loop (Chunks (x:xs)) = yield (Just x) (Chunks (x:xs))
  loop EOF = yield Nothing EOF
-}

-- |             
{-
trc :: (Show a) => String -> a -> b -> b
trc str a b = trace (str ++ ":" ++ show a) b 
-}                     
-- |

flipap :: a -> (a -> b) -> b
flipap = flip ($)

-- | 

unit :: (Monad m) => m () 
unit = return ()

-- | 
 
skipspace :: T.Text -> T.Text 
skipspace = T.dropWhile (\c->(c==' ') || (c=='\n') || (c=='\r'))       
    
-- | 

many0event :: Monad m => 
              (T.Text,T.Text) 
              -> (Event -> Sink Event m (Either String a))
              -> Sink Event m (Either String [a])
many0event (start,end) iter = many1eventWrkr (start,end) id iter 


-- | 

many1event :: Monad m => 
              (T.Text,T.Text) 
              -> (Event -> Sink Event m (Either String a))
              -> Sink Event m (Either String [a])
many1event (start,end) iter = do
  dropWhile (not.isStart start)
  CL.head >>= 
    maybe (return (Left ("error in " ++ T.unpack start))) 
          (\ev -> do ex <- iter ev
                     case ex of       
                       Left err -> return (Left err) 
                       Right x -> 
                         let acc = (x:)
                         in many1eventWrkr (start,end) acc iter)
  
-- |    
  
many1eventWrkr :: Monad m => 
                  (T.Text,T.Text) 
                  -> ( [a] -> [a]  ) 
                  -> (Event -> Sink Event m (Either String a))
                  -> Sink Event m (Either String [a])         
many1eventWrkr (start,end) acc iter = 
    drop2NextStartOrEnd >>= \e -> do 
      case e of 
        Left (txt,ev) -> 
          if txt == start 
          then do CL.drop 1 
                  ex <- iter ev
                  case ex of 
                    Left err -> return (Left err)
                    Right x -> 
                      many1eventWrkr (start,end) (acc.(x:)) iter  
          else return (Left ("got " ++ T.unpack txt))
        Right txt -> 
          if txt == end 
          then do return (Right (acc [])) 
          else return (Left ("got " ++ T.unpack txt))
  
  
-- | 

drop2NextStartOrEnd :: (Monad m) => 
                       Sink Event m (Either (T.Text,Event) T.Text)
drop2NextStartOrEnd = do 
  dropWhile (not.isEventStartEnd)
  melm <- peek 
  case melm of  
    Just elm@(EventBeginElement name _) 
      -> return (Left (nameLocalName name,elm))
    Just (EventEndElement name) 
      -> return (Right (nameLocalName name))
    Just _ -> error "this is impossible in drop2NextStartOrEnd"
    Nothing -> error "no more item in drop2NextStartOrEnd" 


-- * parsers 

-- | parse whole xournal file 

pXournal :: Monad m => Sink Event m (Either String Xournal)
pXournal = do  
  dropWhile (not.isStart "xournal")
  CL.head >>= maybe (
    return (Left "no xournal"))
    (const $ do 
       title <- pTitle 
       pages <- many1event ("page","xournal") pPage
       (return $ Xournal <$> title <*> pages ))  
 
-- | parse one page 

pPage :: Monad m => Event -> Sink Event m (Either String Page)
pPage ev = do let dim = getDimension ev 
              bkg <- pBkg
              layers <- many1event ("layer","page") pLayer 
              dropWhile (not.isEnd "page")
              CL.drop 1 
              return (Page <$> dim <*> bkg <*> layers )  

-- | 
              
pTitle :: Monad m => Sink Event m (Either String S.ByteString) 
pTitle = do dropWhile (not.isStart "title")
            CL.drop 1 
            CL.head >>= 
              maybe (return (Left "not title"))
                    (\ev -> do let title = getContent ev 
                               dropWhile (not.isEnd "title")
                               CL.drop 1 
                               return (encodeUtf8 <$> title) )

-- | 

pBkg :: Monad m => Sink Event m (Either String Background) 
pBkg = do dropWhile (not.isStart "background")
          -- CL.drop 1 
          CL.head >>= 
            maybe (return (Left "not background"))
                  (\ev -> do let bkg = getBackground ev 
                             dropWhile (not.isEnd "background")
                             CL.drop 1 
                             return bkg) 

-- | 
          
pLayer :: Monad m => Event -> Sink Event m (Either String Layer) 
pLayer _ev = do strokes <- many0event ("stroke","layer") pStroke 
                dropWhile (not.isEnd "layer")
                CL.drop 1 
                return (Layer <$> strokes)

-- | 
               
pStroke :: Monad m => Event -> Sink Event m (Either String Stroke) 
pStroke ev = do 
  let estr1wdth = getStroke ev 
  -- trc "pStroke" estr1wdth unit 
  CL.head >>= 
    maybe (return (Left "pStroke ecoord"))
          (\elm -> do 
              let txt = getContent elm :: Either String T.Text
                  ctnt = getStrokeContent id =<< txt 
              dropWhile (not.isEnd "stroke") 
              CL.drop 1 
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
                    -> T.Text
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
            -> T.Text 
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
    f acc@(Right bkg@(BackgroundPdf t d fi p)) (name,contents) =            
      if nameLocalName name == "type" 
      then let ContentText txt = Prelude.head contents 
           in if txt == "pdf" 
              then Right (toBkgPdf bkg)
              else Right (toBkgNoPdf (encodeUtf8 txt) bkg)
      else if nameLocalName name == "domain"           
      then let ContentText txt = Prelude.head contents 
           in Right . (\x -> BackgroundPdf t x fi p) . Just . encodeUtf8 $ txt 
      else if nameLocalName name == "filename" 
      then let ContentText txt = Prelude.head contents 
           in Right . (\x -> BackgroundPdf t d x p) . Just . encodeUtf8 $ txt 
      else if nameLocalName name == "pageno" 
      then let ContentText txt = Prelude.head contents 
           in (\x -> BackgroundPdf t d fi x) . fst <$> decimal txt 
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
getDimension r = Left ("not a dimension : " ++ show r)

-- | get Content   

getContent :: Event -> Either String T.Text
getContent (EventContent (ContentText txt)) = Right txt 
getContent r = Left ("no content" ++ show r)
  

-- * predicates 

-- | 

isEventStartEnd :: Event -> Bool 
isEventStartEnd (EventBeginElement _ _) = True
isEventStartEnd (EventEndElement _ ) = True
isEventStartEnd _ = False 

-- | check start of element with name txt  
  
isStart :: T.Text -> Event -> Bool 
isStart txt (EventBeginElement name _) = nameLocalName name == txt
isStart _ _ = False 

-- | check end of element with name txt 

isEnd :: T.Text -> Event -> Bool
isEnd txt (EventEndElement name) = nameLocalName name == txt 
isEnd _ _ = False 


-- * driver routines 

-- | generic xml file driver

parseXmlFile :: (MonadThrow m, MonadIO m) => Handle -> Sink Event m a -> m a
parseXmlFile h iter = sourceHandle h =$= parseBytes def $$ iter 
  -- enumHandle 4096 h $$ joinI $ parseBytes def $$ iter 


-- | for xournal 

parseXojFile :: FilePath -> IO (Either String Xournal)
parseXojFile fp = withFile fp ReadMode $ \ih -> parseXmlFile ih pXournal     

-- | 

parseXojGzFile :: FilePath -> IO (Either String Xournal) 
parseXojGzFile fp = withFile fp ReadMode $ \h -> 
                      sourceHandle h =$= ungzip =$= parseBytes def $$ pXournal 
 --  run_ $ enumFile fp $$ EZ.ungzip =$ parseBytes def =$ pXournal 

-- | 

parseXournal :: FilePath -> IO (Either String Xournal)
parseXournal fname = 
    checkIfBinary fname >>= \b -> 
      if b then parseXojGzFile fname else parseXojFile fname
  

-- | printing for debug

iterPrint :: (Show s,MonadIO m) => Sink s m () 
iterPrint = do
  x <- CL.head
  maybe (return ()) (liftIO . print >=> \_ -> iterPrint) x



