{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}

module Graphics.Xournal.Render.PDFBackground where

import Control.Monad.State

import Data.ByteString
import Data.ByteString.Char8
import Graphics.Xournal.Type.Generic
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import Text.Xournal.Type

import Control.Monad.Trans
import Prelude hiding (putStrLn)

data BackgroundPDFDrawable = 
  BkgPDFSolid { bkgpdf_color :: ByteString
              , bkgpdf_style :: ByteString
              }
  | forall p. (Poppler.PageClass p) => 
    BkgPDFPDF { bkgpdf_domain :: Maybe ByteString
              , bkgpdf_filename :: Maybe ByteString
              , bkgpdf_pageno :: Int 
              , bkgpdf_popplerpage :: Maybe p 
              } 
  
data Context = Context { ctxt_domain :: ByteString
                       , ctxt_filename :: ByteString 
--                       , ctxt_doc :: Poppler.Document
                       }
    
type TPageBBoxMapBkgPDF = TPageBBoxMapBkg BackgroundPDFDrawable 

type TXournalBBoxMapBkgPDF = TXournalBBoxMapBkg BackgroundPDFDrawable

mkTXournalBBoxMapBkgPDF :: Xournal -> IO TXournalBBoxMapBkgPDF
mkTXournalBBoxMapBkgPDF xoj = do 
  let pgs = xoj_pages xoj 
  npgs <- mkAllTPageBBoxMapBkgPDF pgs 
  return $ GXournal (gFromList npgs)
  
mkAllTPageBBoxMapBkgPDF :: [Page] -> IO [TPageBBoxMapBkgPDF]
mkAllTPageBBoxMapBkgPDF pgs = evalStateT (mapM mkPagePDF pgs) Nothing 


mkPagePDF :: Page -> StateT (Maybe Context) IO TPageBBoxMapBkgPDF
mkPagePDF pg = do  
  let bkg = page_bkg pg
      dim = page_dim pg 
      ls = page_layers pg
  newbkg <- mkBkgPDF bkg
  return $ GPage dim (GBackground newbkg) 
                     (gFromList . Prelude.map fromLayer $ ls)
  
translateBkg :: Background -> BackgroundPDFDrawable
translateBkg (Background _t c s) = BkgPDFSolid c s
translateBkg (BackgroundPdf _t md mf pn) = BkgPDFPDF md mf pn (Nothing :: Maybe Poppler.Page)

mkBkgPDF :: Background 
            -> StateT (Maybe Context) IO BackgroundPDFDrawable
mkBkgPDF bkg = do  
  let bkgpdf = translateBkg bkg
  case bkgpdf of 
    BkgPDFSolid _ _ -> return bkgpdf 
    BkgPDFPDF md mf pn _ -> do 
      mctxt <- get 
      newbkgpdf <- case mctxt of
                     Nothing -> do 
                       case (md,mf) of 
                         (Just d, Just f) -> do 
                           put $ Just (Context d f)
                           liftIO $ putStrLn "hey"
                         _ -> return ()
                       return bkgpdf
                     Just (Context oldd oldf) -> do 
                       -- not supporting changing file. 
                       let (nd,nf) = case (md,mf) of 
                                       (Nothing,Nothing) -> (oldd,oldf)
                                       (Just d,Nothing)  -> (oldd,oldf)
                                       (Nothing,Just f)  -> (oldd,oldf)
                                       (Just d,Just f)   -> (oldd,oldf)
                       put $ Just (Context nd nf)
                       return $ BkgPDFPDF (Just nd) (Just nf) pn (Nothing :: Maybe Poppler.Page )  
      -- not finished yet
      return (newbkgpdf)