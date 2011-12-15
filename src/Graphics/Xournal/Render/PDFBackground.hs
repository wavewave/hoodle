{-# LANGUAGE ExistentialQuantification, OverloadedStrings, 
             FlexibleInstances, FlexibleContexts #-}

module Graphics.Xournal.Render.PDFBackground where

import Control.Monad.State

import Data.Monoid 
import Data.ByteString hiding (putStrLn)
import qualified Data.ByteString.Char8 as C
import Graphics.Xournal.Type.Generic
import Graphics.Xournal.Render.Generic

import Text.Xournal.Type

import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage

import Graphics.Rendering.Cairo
-- import Control.Monad.Trans
-- import Prelude -- hiding (putStrLn)

data BackgroundPDFDrawable = 
  BkgPDFSolid { bkgpdf_color :: ByteString
              , bkgpdf_style :: ByteString
              }
  | -- (Poppler.PageClass p) => 
    BkgPDFPDF { bkgpdf_domain :: Maybe ByteString
              , bkgpdf_filename :: Maybe ByteString
              , bkgpdf_pageno :: Int 
              , bkgpdf_popplerpage :: Maybe Poppler.Page -- Maybe p 
              } 
  
data Context = Context { ctxt_domain :: ByteString
                       , ctxt_filename :: ByteString 
                       , ctxt_doc :: Maybe Poppler.Document
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
      case mctxt of
        Nothing -> do 
          case (md,mf) of 
            (Just d, Just f) -> do 
              liftIO $ putStrLn "hey"
              mdoc <- popplerGetDocFromFile f
              put $ Just (Context d f mdoc)
              case mdoc of 
                Just doc -> do  
                  mpg <- popplerGetPageFromDoc doc pn 
                  return (bkgpdf {bkgpdf_popplerpage = mpg}) 
            _ -> return bkgpdf 
        Just (Context oldd oldf olddoc) -> do 
          mpage <- case olddoc of 
            Just doc -> do 
              popplerGetPageFromDoc doc pn
            Nothing -> return Nothing 
          return $ BkgPDFPDF (Just oldd) (Just oldf) pn mpage

            
      -- not finished yet
            
            
      -- return (newbkgpdf)

          -- not supporting changing file. 
            --let (nd,nf) = case (md,mf) of 
            --               (Nothing,Nothing) -> (oldd,oldf)
            --                (Just d,Nothing)  -> (oldd,oldf)
            --                (Nothing,Just f)  -> (oldd,oldf)
            --                (Just d,Just f)   -> (oldd,oldf)
            -- put $ Just (Context nd nf olddoc)
popplerGetDocFromFile :: (MonadIO m) => 
                         ByteString -> m (Maybe Poppler.Document)
popplerGetDocFromFile fp = 
  liftIO $ Poppler.documentNewFromFile 
             (C.unpack ("file://localhost" `mappend` fp)) Nothing 
             
             
popplerGetPageFromDoc :: (MonadIO m) => 
                         Poppler.Document -> Int -> m (Maybe Poppler.Page)
popplerGetPageFromDoc doc pn = do   
  n <- liftIO $ Poppler.documentGetNPages doc  
  liftIO $ putStrLn $ "pages : " ++ (show n)
  liftIO $ putStrLn $ "current page = " ++ show pn
  pg <- liftIO $ Poppler.documentGetPage doc (pn-1) 
  return (Just pg)


instance (Renderable (b,Dimension)) => 
         Renderable (GBackground b,Dimension) where
  cairoRender (GBackground b,dim) = cairoRender (b,dim)

cairoRenderBackgroundPDFDrawable :: (BackgroundPDFDrawable,Dimension) 
                                    -> Render ()
cairoRenderBackgroundPDFDrawable (BkgPDFSolid c s,dim) = 
  cairoRender (Background "solid" c s,dim)
cairoRenderBackgroundPDFDrawable (BkgPDFPDF _ _ _ p,dim) = do
  case p of 
    Nothing -> return () 
    Just pg -> PopplerPage.pageRender pg
    
instance Renderable (BackgroundPDFDrawable,Dimension) where
  cairoRender = cairoRenderBackgroundPDFDrawable
  
