{-# LANGUAGE ExistentialQuantification, OverloadedStrings, 
             FlexibleInstances, FlexibleContexts,  
             TypeFamilies #-}

module Graphics.Xournal.Render.PDFBackground where

import Control.Monad.State

import Data.Monoid 
import Data.ByteString hiding (putStrLn)
import qualified Data.ByteString.Char8 as C
import Data.Xournal.Generic
import Data.Xournal.Simple
import Data.Xournal.BBox
import Graphics.Xournal.Render.BBoxMapPDF
import Graphics.Xournal.Render.Generic
import Graphics.Xournal.Render.Type


import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage

import Graphics.Rendering.Cairo

  
data Context = Context { ctxt_domain :: ByteString
                       , ctxt_filename :: ByteString 
                       , ctxt_doc :: Maybe Poppler.Document
                       }


mkTXournalBBoxMapPDF :: Xournal -> IO TXournalBBoxMapPDF
mkTXournalBBoxMapPDF xoj = do 
  let pgs = xoj_pages xoj 
  npgs <- mkAllTPageBBoxMapPDF pgs 
  return $ GXournal (xoj_title xoj) (gFromList npgs)
  
mkAllTPageBBoxMapPDF :: [Page] -> IO [TPageBBoxMapPDF]
mkAllTPageBBoxMapPDF pgs = evalStateT (mapM mkPagePDF pgs) Nothing 


mkPagePDF :: Page -> StateT (Maybe Context) IO TPageBBoxMapPDF
mkPagePDF pg = do  
  let bkg = page_bkg pg
      dim = page_dim pg 
      ls = page_layers pg
  newbkg <- mkBkgPDF bkg
  return $ GPage dim newbkg (gFromList . Prelude.map fromLayer $ ls)
  
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
          return $ BkgPDFPDF md mf pn mpage
            
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


instance RenderOptionable (BackgroundPDFDrawable,Dimension) where
  type RenderOption (BackgroundPDFDrawable,Dimension) = BkgPDFOption 
  cairoRenderOption DrawBkgPDF (b,dim) = cairoRenderBackgroundPDFDrawable (b,dim)
  cairoRenderOption DrawWhite (_,Dim w h) = do 
    setSourceRGBA 1 1 1 1
    rectangle 0 0 w h 
    fill 



cairoDrawPageBBoxPDF :: Maybe BBox -> TPageBBoxMapPDF -> Render ()
cairoDrawPageBBoxPDF mbbox page = do 
  cairoRender page 
{-  case gbackground page of 
    BkgPDFSolid c s -> do 
      let bkg = Background "solid" c s 
          bkg 
      
      page
-}    
    
  








