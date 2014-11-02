{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Util 
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Util where

import Control.Applicative 
import Control.Monad.Trans.Maybe
import Data.Attoparsec.Char8 
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Time.Clock 
import Data.Time.Format
import Data.UUID.V4 (nextRandom)
import Network.URI
import System.Directory 
import System.Environment 
import System.FilePath
import System.IO
import System.Locale
-- 
import Data.Hoodle.Simple
-- 
-- import Debug.Trace

(#) :: a -> (a -> b) -> b 
(#) = flip ($)
infixr 0 #

maybeFlip :: Maybe a -> b -> (a->b) -> b  
maybeFlip m n j = maybe n j m   

maybeRead :: Read a => String -> Maybe a 
maybeRead = fmap fst . listToMaybe . reads 

runMaybeT_ :: (Monad m) => MaybeT m a -> m ()
runMaybeT_ m = runMaybeT m >> return () 

fromJustError :: String -> Maybe a -> a
fromJustError _   (Just x) = x
fromJustError err Nothing = error err

either_ :: (Monad m) => (b -> m ()) -> Either a b -> m ()
either_ = either (const (return ()))


uncurry4 :: (a->b->c->d->e)->(a,b,c,d)->e 
uncurry4 f (x,y,z,w) = f x y z w 

getLargestWidth :: Hoodle -> Double 
getLargestWidth hdl = 
  let ws = map (dim_width . page_dim) (hoodle_pages hdl)  
  in  maximum ws 

getLargestHeight :: Hoodle -> Double 
getLargestHeight hdl = 
  let hs = map (dim_height . page_dim) (hoodle_pages hdl)  
  in  maximum hs 

waitUntil :: (Monad m) => (a -> Bool) -> m a -> m ()
waitUntil p act = do 
  a <- act
  if p a
    then return ()
    else waitUntil p act  

-- | for debugging
errorlog :: String -> IO ()
errorlog str = do 
  homepath <- getEnv "HOME"
  let dir = homepath </> ".hoodle.d"
  createDirectoryIfMissing False dir
  outh <- openFile (dir </> "error.log") AppendMode 
  utctime <- getCurrentTime 
  let timestr = formatTime defaultTimeLocale "%F %H:%M:%S %Z" utctime
  hPutStr outh (timestr ++ " : " )  
  hPutStrLn outh str
  hClose outh 

-- | 
maybeError' :: String -> Maybe a -> a
maybeError' str = maybe (error str) id

data UrlPath = FileUrl FilePath | HttpUrl String 
             deriving (Show,Eq)

data T = N | F | H | HS deriving (Show,Eq)

-- | 
urlParse :: String -> Maybe UrlPath 
urlParse str = 
  if length str < 7 
    then Just (FileUrl str) 
    else 
      let p = do b <- (try (string "file://" *> return F)  
                       <|> try (string "http://" *> return H) 
                       <|> try (string "https://" *> return HS)
                       <|> (return N) )
                 remaining <- manyTill anyChar ((satisfy (inClass "\r\n") *> return ()) <|> endOfInput)
                 return (b,remaining) 
          r = parseOnly p (B.pack str)
      in case r of 
           Left _ -> Nothing  
           Right (b,f) -> case b of 
                            N -> Just (FileUrl f)
                            F -> Just (FileUrl (unEscapeString f))
                            H -> Just (HttpUrl ("http://" ++ f))
                            HS -> Just (HttpUrl ("https://" ++ f))
    

-- | 
mkTmpFile :: String -> IO FilePath
mkTmpFile ext = do 
  tdir <- getTemporaryDirectory
  tuuid <- nextRandom
  return $ tdir </> show tuuid <.> ext
