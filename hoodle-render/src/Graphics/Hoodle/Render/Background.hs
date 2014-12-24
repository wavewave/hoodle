{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.PDFBackground 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Background where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.State hiding (mapM_)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader (ask)
import           Data.ByteString hiding (putStrLn,filter)
import           Data.Foldable (mapM_)
import qualified Data.Map as M
import           Data.ByteString.Base64 
import qualified Data.ByteString.Char8 as C
import           Data.Monoid
import           Data.UUID.V4 (nextRandom)
import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.UI.Gtk (postGUIAsync)
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler
import qualified Graphics.UI.Gtk.Poppler.Page as PopplerPage
import           System.Directory
import           System.FilePath ((</>),(<.>))
-- from hoodle-platform
import           Data.Hoodle.BBox
import           Data.Hoodle.Predefined 
import           Data.Hoodle.Simple
--
import           Graphics.Hoodle.Render.Type.Background
import           Graphics.Hoodle.Render.Type.Renderer
-- 
import Prelude hiding (mapM_)

-- |
popplerGetDocFromFile :: ByteString -> IO (Maybe Poppler.Document)
popplerGetDocFromFile fp = 
  Poppler.documentNewFromFile 
    (C.unpack ("file://localhost" `mappend` fp)) Nothing 

-- |
getByteStringIfEmbeddedPDF :: ByteString -> Maybe ByteString 
getByteStringIfEmbeddedPDF bstr = do 
    guard (C.length bstr > 30)
    let (header,dat) = C.splitAt 30 bstr 
    guard (header == "data:application/x-pdf;base64,") 
    either (const Nothing) return (decode dat)

-- | 
popplerGetDocFromDataURI :: ByteString -> IO (Maybe Poppler.Document) 
popplerGetDocFromDataURI dat = do 
  let mdecoded = getByteStringIfEmbeddedPDF dat 
  case mdecoded of 
    Nothing -> return Nothing 
    Just decoded -> do 
      uuidstr <- liftM show nextRandom
      tmpdir <- getTemporaryDirectory 
      let tmpfile = tmpdir </> uuidstr <.> "pdf" 
      C.writeFile tmpfile decoded 
      mdoc <- popplerGetDocFromFile (C.pack tmpfile)
      removeFile tmpfile 
      return mdoc 

-- |
popplerGetPageFromDoc :: Poppler.Document 
                      -> Int -- ^ page number 
                      -> IO (Maybe Poppler.Page)
popplerGetPageFromDoc doc pn = do   
  n <- Poppler.documentGetNPages doc 
  if pn > n 
    then return Nothing
    else do 
      pg <- Poppler.documentGetPage doc (pn-1) 
      return (Just pg)

-- | draw ruling all 
drawRuling :: Double -> Double -> ByteString -> Cairo.Render () 
drawRuling w h style = do
  let drawHorizRules = do 
      let (r,g,b,a) = predefined_RULING_COLOR         
      Cairo.setSourceRGBA r g b a 
      Cairo.setLineWidth predefined_RULING_THICKNESS
      let drawonerule y = do 
            Cairo.moveTo 0 y 
            Cairo.lineTo w y
            Cairo.stroke  
      mapM_ drawonerule [ predefined_RULING_TOPMARGIN 
                        , predefined_RULING_TOPMARGIN+predefined_RULING_SPACING
                        .. 
                        h-1 ]
  case style of 
    "plain" -> return () 
    "lined" -> do 
      drawHorizRules
      let (r2,g2,b2,a2) = predefined_RULING_MARGIN_COLOR
      Cairo.setSourceRGBA r2 g2 b2 a2 
      Cairo.setLineWidth predefined_RULING_THICKNESS
      Cairo.moveTo predefined_RULING_LEFTMARGIN 0 
      Cairo.lineTo predefined_RULING_LEFTMARGIN h
      Cairo.stroke
    "ruled" -> drawHorizRules 
    "graph" -> do 
      let (r3,g3,b3,a3) = predefined_RULING_COLOR 
      Cairo.setSourceRGBA r3 g3 b3 a3 
      Cairo.setLineWidth predefined_RULING_THICKNESS
      let drawonegraphvert x = do 
            Cairo.moveTo x 0 
            Cairo.lineTo x h
            Cairo.stroke  
      let drawonegraphhoriz y = do 
            Cairo.moveTo 0 y
            Cairo.lineTo w y
            Cairo.stroke
      mapM_ drawonegraphvert  [0,predefined_RULING_GRAPHSPACING..w-1] 
      mapM_ drawonegraphhoriz [0,predefined_RULING_GRAPHSPACING..h-1]
    _ -> return ()     



-- | draw ruling  in bbox 
drawRuling_InBBox :: BBox -> Double -> Double -> ByteString -> Cairo.Render () 
drawRuling_InBBox (BBox (x1,y1) (x2,y2)) w h style = do
  let drawonerule y = do 
        Cairo.moveTo x1 y 
        Cairo.lineTo x2 y
        Cairo.stroke  
  let drawonegraphvert x = do 
        Cairo.moveTo x y1 
        Cairo.lineTo x y2
        Cairo.stroke  
  let drawonegraphhoriz y = do 
        Cairo.moveTo x1 y
        Cairo.lineTo x2 y
        Cairo.stroke
      fullRuleYs = [ predefined_RULING_TOPMARGIN 
                   , predefined_RULING_TOPMARGIN+predefined_RULING_SPACING
                   .. 
                   h-1 ]
      ruleYs = filter (\y-> (y <= y2) && (y >= y1)) fullRuleYs
      fullGraphXs = [0,predefined_RULING_GRAPHSPACING..w-1]          
      fullGraphYs = [0,predefined_RULING_GRAPHSPACING..h-1]
      graphXs = filter (\x->(x<=x2)&&(x>=x1)) fullGraphXs
      graphYs = filter (\y->(y<=y2)&&(y>=y1)) fullGraphYs 
  let drawHorizRules = do 
      let (r,g,b,a) = predefined_RULING_COLOR         
      Cairo.setSourceRGBA r g b a 
      Cairo.setLineWidth predefined_RULING_THICKNESS
      mapM_ drawonerule ruleYs
  case style of 
    "plain" -> return () 
    "lined" -> do 
      drawHorizRules
      let (r2,g2,b2,a2) = predefined_RULING_MARGIN_COLOR
      Cairo.setSourceRGBA r2 g2 b2 a2 
      Cairo.setLineWidth predefined_RULING_THICKNESS
      Cairo.moveTo predefined_RULING_LEFTMARGIN 0 
      Cairo.lineTo predefined_RULING_LEFTMARGIN h
      Cairo.stroke
    "ruled" -> drawHorizRules 
    "graph" -> do 
      let (r3,g3,b3,a3) = predefined_RULING_COLOR 
      Cairo.setSourceRGBA r3 g3 b3 a3 
      Cairo.setLineWidth predefined_RULING_THICKNESS
      mapM_ drawonegraphvert  graphXs 
      mapM_ drawonegraphhoriz graphYs
    _ -> return ()     


-- | render background without any constraint 
renderBkg :: (Background,Dimension) -> Cairo.Render () 
renderBkg (Background _typ col sty,Dim w h) = do 
    let c = M.lookup col predefined_bkgcolor  
    case c of 
      Just (r,g,b,_a) -> Cairo.setSourceRGB r g b 
      Nothing         -> Cairo.setSourceRGB 1 1 1 
    Cairo.rectangle 0 0 w h 
    Cairo.fill
    drawRuling w h sty
renderBkg (BackgroundPdf _ _ _ _,Dim w h) = do 
    Cairo.setSourceRGBA 1 1 1 1
    Cairo.rectangle 0 0 w h 
    Cairo.fill
renderBkg (BackgroundEmbedPdf _ _,Dim w h) = do 
    Cairo.setSourceRGBA 1 1 1 1
    Cairo.rectangle 0 0 w h 
    Cairo.fill



-- | this has some bugs. need to fix 
cnstrctRBkg_StateT :: Dimension 
                   -> Background 
                   -> StateT (Maybe Context) Renderer RBackground
cnstrctRBkg_StateT dim@(Dim w h) bkg = do  
  (handler,qvar) <- lift ask
  uuid <- liftIO nextRandom
  case bkg of 
    Background _t c s -> return (RBkgSmpl c s uuid) 
    BackgroundPdf _t md mf pn -> do 
      r <- runMaybeT $ do
        (pg,rbkg) <- case (md,mf) of 
          (Just d, Just f) -> do 
            uuiddoc <- liftIO nextRandom
            docvar <- liftIO (atomically newEmptyTMVar)
            liftIO . atomically $ sendPDFCommand uuiddoc qvar (GetDocFromFile f docvar)
            doc <- MaybeT . liftIO $ atomically $ takeTMVar docvar 
            lift . put $ Just (Context d f (Just doc) Nothing)
            --
            uuidpg <- liftIO nextRandom
            pgvar <- liftIO (atomically newEmptyTMVar)
            liftIO . atomically $ sendPDFCommand uuidpg qvar (GetPageFromDoc doc pn pgvar)
            pg <- MaybeT . liftIO $ atomically $ takeTMVar pgvar        
            return (pg, RBkgPDF md f pn (Just pg) uuid)
          _ -> do 
            Context oldd oldf olddoc _ <- MaybeT get
            doc <- MaybeT . return $ olddoc  
            uuidpg <- liftIO nextRandom
            pgvar <- liftIO (atomically newEmptyTMVar)
            liftIO . atomically $ sendPDFCommand uuidpg qvar (GetPageFromDoc doc pn pgvar)
            pg <- MaybeT . liftIO $ atomically $ takeTMVar pgvar        
            return (pg, RBkgPDF (Just oldd) oldf pn (Just pg) uuid)
        return rbkg
      case r of
        Nothing -> error "error in cnstrctRBkg_StateT"
        Just x -> return x
    BackgroundEmbedPdf _ pn -> do 
      r <- runMaybeT $ do 
        Context _ _ _ mdoc <- MaybeT get
        liftIO $ putStrLn "1"
        doc <- (MaybeT . return) mdoc 
        liftIO $ putStrLn "2"
        uuidpg <- liftIO nextRandom
        liftIO $ putStrLn "3"
        pgvar <- liftIO (atomically newEmptyTMVar)
        liftIO $ putStrLn "4"
        liftIO . atomically $ sendPDFCommand uuidpg qvar (GetPageFromDoc doc pn pgvar)
        liftIO $ putStrLn "5"
        pg <- MaybeT . liftIO $ atomically $ takeTMVar pgvar        
        liftIO $ putStrLn "6"
        return (RBkgEmbedPDF pn (Just pg) uuid)
      case r of 
        Nothing -> error "error in cnstrctRBkg_StateT"
        Just x -> return x

 
-- | For simple hoodle background
renderBackground_StateT :: Dimension -> Background -> StateT Context Cairo.Render ()
renderBackground_StateT dim@(Dim w h) bkg = do
    case bkg of 
      Background _t c s -> lift (renderBkg (bkg,dim))
      BackgroundPdf _t md mf pn -> do 
        r <- runMaybeT $ do
          case (md,mf) of 
            (Just d, Just f) -> do
              doc <- (MaybeT . liftIO . popplerGetDocFromFile) f
              lift . put $ (Context d f (Just doc) Nothing)
              pg <- MaybeT . liftIO $ popplerGetPageFromDoc doc pn
              lift . lift $ pdfRender pg
            _ -> do 
              Context oldd oldf olddoc _ <- lift get
              doc <- MaybeT . return $ olddoc  
              pg <- MaybeT . liftIO $ popplerGetPageFromDoc doc pn
              lift . lift $ pdfRender pg
        maybe (error "renderBackground_StateT") (const (return ())) r
      BackgroundEmbedPdf _ pn -> do 
        r <- runMaybeT $ do 
          Context _ _ _ mdoc <- lift get
          doc <- (MaybeT . return) mdoc 
          pg <- MaybeT . liftIO $ popplerGetPageFromDoc doc pn
          lift . lift $ pdfRender pg
        maybe (error "renderBackground_StateT") (const (return ())) r
  where pdfRender pg = do Cairo.setSourceRGBA 1 1 1 1
                          Cairo.rectangle 0 0 w h
                          Cairo.fill
                          PopplerPage.pageRender pg
