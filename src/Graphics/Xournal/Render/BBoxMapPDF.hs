{-# LANGUAGE OverloadedStrings, TypeFamilies, TypeSynonymInstances, MultiParamTypeClasses #-}

module Graphics.Xournal.Render.BBoxMapPDF where

import Graphics.Xournal.Render.Type 
import Data.Xournal.Simple
import Data.Xournal.Generic
import Data.Xournal.Map
import Data.Xournal.BBox
import Data.Xournal.Buffer
import Data.IntMap
import Data.Traversable 

import Control.Category
import Data.Label
import Prelude hiding ((.),id,mapM)

import Data.ByteString hiding (putStrLn, empty)
import qualified Data.ByteString.Char8 as C hiding (empty)
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler

import Graphics.Rendering.Cairo

data BackgroundPDFDrawable = 
  BkgPDFSolid { bkgpdf_color :: ByteString
              , bkgpdf_style :: ByteString
              }
  | BkgPDFPDF { bkgpdf_domain :: Maybe ByteString
              , bkgpdf_filename :: Maybe ByteString
              , bkgpdf_pageno :: Int 
              , bkgpdf_popplerpage :: Maybe Poppler.Page 
              , bkgpdf_cairosurface :: Maybe Surface
              } 

data BkgPDFOption = DrawBkgPDF 
                  | DrawWhite 
                  | DrawBuffer
                  | DrawPDFInBBox (Maybe BBox)

type TPageBBoxMapPDF = TPageBBoxMapBkg BackgroundPDFDrawable 

type TXournalBBoxMapPDF = TXournalBBoxMapBkg BackgroundPDFDrawable

type TTempPageSelectPDF = GPage BackgroundPDFDrawable (TLayerSelectInPage []) TLayerBBox

type TTempXournalSelectPDF = GSelect (IntMap TPageBBoxMapPDF) (Maybe (Int, TTempPageSelectPDF))


instance GBackgroundable BackgroundPDFDrawable where 
  gFromBackground = bkgPDFFromBkg 
  gToBackground = bkgFromBkgPDF

bkgFromBkgPDF :: BackgroundPDFDrawable -> Background 
bkgFromBkgPDF (BkgPDFSolid c s) = Background "solid" c s 
bkgFromBkgPDF (BkgPDFPDF d f n _ _ ) = BackgroundPdf "pdf" d f n 

bkgPDFFromBkg :: Background -> BackgroundPDFDrawable
bkgPDFFromBkg (Background _t c s) = BkgPDFSolid c s
bkgPDFFromBkg (BackgroundPdf _t md mf pn) = BkgPDFPDF md mf pn Nothing Nothing


{-  
emptyTXournalBBoxMapPDF :: TXournalBBoxMapPDF
emptyTXournalBBoxMapPDF = GXournal "" empty
-}

tlayerBBoxFromTLayerSelect :: TLayerSelect TLayerBBox -> TLayerBBox 
tlayerBBoxFromTLayerSelect l = 
  case unTEitherAlterHitted (gstrokes l) of
    Left strs -> GLayer strs 
    Right alist -> GLayer . Prelude.concat $ interleave id unHitted alist

tlayerbufFromTLayerSelectBuf :: TLayerSelectBuf (TLayerBBoxBuf b) 
                                -> (TLayerBBoxBuf b) 
tlayerbufFromTLayerSelectBuf l = 
  case unTEitherAlterHitted (get g_bstrokes l) of
    Left strs -> GLayerBuf (get g_buffer l) strs 
    Right alist -> GLayerBuf (get g_buffer l) . Prelude.concat 
                   $ interleave id unHitted alist
  
instance GCast TTempPageSelectPDF TPageBBoxMapPDF where      
  gcast = tpageBBoxMapPDFFromTTempPageSelectPDF


tpageBBoxMapPDFFromTTempPageSelectPDF :: TTempPageSelectPDF -> TPageBBoxMapPDF
tpageBBoxMapPDFFromTTempPageSelectPDF p = 
  let TLayerSelectInPage s others = glayers p 
      s' = tlayerBBoxFromTLayerSelect s
  in  GPage (gdimension p) (gbackground p) (gFromList (s':others))
     
instance GCast TTempPageSelectPDFBuf TPageBBoxMapPDFBuf where      
  gcast = tpageBBoxMapPDFBufFromTTempPageSelectPDFBuf
      
tpageBBoxMapPDFBufFromTTempPageSelectPDFBuf :: TTempPageSelectPDFBuf 
                                               -> TPageBBoxMapPDFBuf
tpageBBoxMapPDFBufFromTTempPageSelectPDFBuf p = 
  let TLayerSelectInPageBuf s others = get g_layers p 
      s' = tlayerbufFromTLayerSelectBuf s
  in  GPage (gdimension p) (gbackground p) (gFromList (s':others))

      
instance GCast TPageBBoxMapPDF TTempPageSelectPDF where
  gcast = ttempPageSelectPDFFromTPageBBoxMapPDF

ttempPageSelectPDFFromTPageBBoxMapPDF :: TPageBBoxMapPDF -> TTempPageSelectPDF 
ttempPageSelectPDFFromTPageBBoxMapPDF p = 
  let (x:xs) = gToList (glayers p)
      l = GLayer . TEitherAlterHitted . Left . gToList . gstrokes $ x
  in  GPage (gdimension p) (gbackground p) (TLayerSelectInPage l xs)
      

instance GCast TPageBBoxMapPDFBuf TTempPageSelectPDFBuf where
  gcast = ttempPageSelectPDFBufFromTPageBBoxMapPDFBuf

ttempPageSelectPDFBufFromTPageBBoxMapPDFBuf :: TPageBBoxMapPDFBuf
                                               -> TTempPageSelectPDFBuf
ttempPageSelectPDFBufFromTPageBBoxMapPDFBuf p = 
  let (x:xs) = gToList (get g_layers p)
      l = GLayerBuf { 
          gbuffer = gbuffer x ,
          gbstrokes = TEitherAlterHitted . Left . gToList . gbstrokes $ x
          }
  in  GPage (gdimension p) (gbackground p) (TLayerSelectInPageBuf l xs)

----------------------      

newtype LyBuf = LyBuf { mbuffer :: Maybe Surface } 

type instance StrokeTypeFromLayer (TLayerBBoxBuf b) = StrokeBBox 

type TPageBBoxMapPDFBuf = 
  TPageBBoxMapBkgBuf BackgroundPDFDrawable LyBuf
  
type TXournalBBoxMapPDFBuf = 
  TXournalBBoxMapBkgBuf BackgroundPDFDrawable LyBuf
  
type TTempPageSelectPDFBuf = 
  GPage BackgroundPDFDrawable (TLayerSelectInPageBuf []) (TLayerBBoxBuf LyBuf)

type TTempXournalSelectPDFBuf = 
  GSelect (IntMap TPageBBoxMapPDFBuf) (Maybe (Int, TTempPageSelectPDFBuf))

mkTLayerBBoxBufFromNoBuf :: TLayerBBox -> IO (TLayerBBoxBuf LyBuf)
mkTLayerBBoxBufFromNoBuf lyr = do 
  let strs = get g_strokes lyr 
  return $ GLayerBuf { gbuffer = LyBuf Nothing, 
                       gbstrokes = strs }  -- temporary

mkTPageBBoxMapPDFBufFromNoBuf :: TPageBBoxMapPDF -> IO TPageBBoxMapPDFBuf
mkTPageBBoxMapPDFBufFromNoBuf page = do 
  let dim = get g_dimension page
      bkg = get g_background page
      ls =  get g_layers page
  ls' <- mapM mkTLayerBBoxBufFromNoBuf ls
  return $ GPage dim bkg ls'
      

mkTXournalBBoxMapPDFBufFromNoBuf :: TXournalBBoxMapPDF 
                                    -> IO TXournalBBoxMapPDFBuf
mkTXournalBBoxMapPDFBufFromNoBuf xoj = do 
  let title = get g_title xoj
      pages = get g_pages xoj 
  pages' <- mapM mkTPageBBoxMapPDFBufFromNoBuf pages
 
  return $ GXournal title pages'

instance GCast (TLayerBBoxBuf a) TLayerBBox where
  gcast = tlayerBBoxFromTLayerBBoxBuf
 
instance GCast TPageBBoxMapPDFBuf TPageBBoxMapPDF where
  gcast = tpageBBoxMapPDFFromTPageBBoxMapPDFBuf

tlayerBBoxFromTLayerBBoxBuf :: TLayerBBoxBuf a -> TLayerBBox
tlayerBBoxFromTLayerBBoxBuf (GLayerBuf _ strs) = GLayer strs 

tpageBBoxMapPDFFromTPageBBoxMapPDFBuf :: TPageBBoxMapPDFBuf -> TPageBBoxMapPDF
tpageBBoxMapPDFFromTPageBBoxMapPDFBuf (GPage dim bkg ls) = 
  GPage dim bkg (fmap tlayerBBoxFromTLayerBBoxBuf ls)