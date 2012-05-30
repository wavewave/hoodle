-----------------------------------------------------------------------------
-- |
-- Module      : Application.Hoodle.Util 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.Hoodle.Util where

import Data.Maybe
import Data.Xournal.Simple

-- import Data.Time.Clock 
-- import Data.Time.Format
-- import System.Locale

-- for test
-- import Blaze.ByteString.Builder
-- import Text.Xournal.Builder 

{-
testPage :: Page Edit -> IO () 
testPage page = do
    let pagesimple = toPage bkgFromBkgPDF . tpageBBoxMapPDFFromTPageBBoxMapPDFBuf $ page 
    L.putStrLn . toLazyByteString . Text.Xournal.Builder.fromPage $ pagesimple 
-}

             
{-
testXournal :: XournalState -> IO () 
testXournal xojstate = do
  let xojsimple :: Xournal = case xojstate of
                               ViewAppendState xoj -> xournalFromTXournalSimple (gcast xoj :: TXournalSimple)
                               SelectState txoj -> xournalFromTXournalSimple (gcast txoj :: TXournalSimple)
  L.putStrLn (builder xojsimple)
-}

maybeFlip :: Maybe a -> b -> (a->b) -> b  
maybeFlip m n j = maybe n j m   

uncurry4 :: (a->b->c->d->e)->(a,b,c,d)->e 
uncurry4 f (x,y,z,w) = f x y z w 

maybeRead :: Read a => String -> Maybe a 
maybeRead = fmap fst . listToMaybe . reads 

maybeError :: String -> Maybe a -> a
maybeError str = maybe (error str) id 

getLargestWidth :: Xournal -> Double 
getLargestWidth xoj = 
  let ws = map (dim_width . page_dim) (xoj_pages xoj)  
  in  maximum ws 

getLargestHeight :: Xournal -> Double 
getLargestHeight xoj = 
  let hs = map (dim_height . page_dim) (xoj_pages xoj)  
  in  maximum hs 

waitUntil :: (Monad m) => (a -> Bool) -> m a -> m ()
waitUntil p act = do 
  a <- act
  if p a
    then return ()
    else waitUntil p act  

-- | for debugging
{-
timeShow :: String -> IO () 
timeShow msg = 
  putStrLn . (msg ++) . (formatTime defaultTimeLocale "%Q") 
    =<< getCurrentTime 
-}