module Application.HXournal.Util where

import Graphics.Xournal.Render.BBoxMapPDF

import Data.Maybe

import Data.Xournal.Generic

import Data.Xournal.Simple
import Blaze.ByteString.Builder
import Graphics.Xournal.Render.PDFBackground
import qualified Data.ByteString.Lazy as L
import Text.Xournal.Builder 


testPage :: TPageBBoxMapPDFBuf -> IO () 
testPage page = do
    let pagesimple = toPage bkgFromBkgPDF . tpageBBoxMapPDFFromTPageBBoxMapPDFBuf $ page 
    L.putStrLn . toLazyByteString . Text.Xournal.Builder.fromPage $ pagesimple  


             
{-
testXournal :: XournalState -> IO () 
testXournal xojstate = do
  let xojsimple :: Xournal = case xojstate of
                               ViewAppendState xoj -> xournalFromTXournalSimple (gcast xoj :: TXournalSimple)
                               SelectState txoj -> xournalFromTXournalSimple (gcast txoj :: TXournalSimple)
  L.putStrLn (builder xojsimple)
-}

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
