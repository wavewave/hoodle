{-# LANGUAGE OverloadedStrings #-}

module Graphics.Xournal.Render.BBoxMapPDF where

import Graphics.Xournal.Render.Type 
import Data.Xournal.Simple
import Data.Xournal.Generic
import Data.Xournal.Map
import Data.Xournal.BBox
import Data.IntMap

import Data.ByteString hiding (putStrLn, empty)
import qualified Data.ByteString.Char8 as C hiding (empty)
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler

data BackgroundPDFDrawable = 
  BkgPDFSolid { bkgpdf_color :: ByteString
              , bkgpdf_style :: ByteString
              }
  | BkgPDFPDF { bkgpdf_domain :: Maybe ByteString
              , bkgpdf_filename :: Maybe ByteString
              , bkgpdf_pageno :: Int 
              , bkgpdf_popplerpage :: Maybe Poppler.Page 
              } 

data BkgPDFOption = DrawBkgPDF | DrawWhite

type TPageBBoxMapPDF = TPageBBoxMapBkg BackgroundPDFDrawable 

type TXournalBBoxMapPDF = TXournalBBoxMapBkg BackgroundPDFDrawable

type TTempPageSelectPDF = GPage BackgroundPDFDrawable (TLayerSelectInPage []) TLayerBBox

type TTempXournalSelectPDF = GSelect (IntMap TPageBBoxMapPDF) (Maybe (Int, TTempPageSelectPDF))


instance GBackgroundable BackgroundPDFDrawable where 
  gFromBackground = bkgPDFFromBkg 
  gToBackground = bkgFromBkgPDF

bkgFromBkgPDF :: BackgroundPDFDrawable -> Background 
bkgFromBkgPDF (BkgPDFSolid c s) = Background "solid" c s 
bkgFromBkgPDF (BkgPDFPDF d f n _ ) = BackgroundPdf "pdf" d f n 

bkgPDFFromBkg :: Background -> BackgroundPDFDrawable
bkgPDFFromBkg (Background _t c s) = BkgPDFSolid c s
bkgPDFFromBkg (BackgroundPdf _t md mf pn) = BkgPDFPDF md mf pn (Nothing :: Maybe Poppler.Page)


  
emptyTXournalBBoxMapPDF :: TXournalBBoxMapPDF
emptyTXournalBBoxMapPDF = GXournal "" empty


tlayerBBoxFromTLayerSelect :: TLayerSelect TLayerBBox -> TLayerBBox 
tlayerBBoxFromTLayerSelect l = 
  case unTEitherAlterHitted (gstrokes l) of
    Left strs -> GLayer strs 
    Right alist -> GLayer . Prelude.concat $ interleave id unHitted alist

tpageBBoxMapPDFFromTTempPageSelectPDF :: TTempPageSelectPDF -> TPageBBoxMapPDF
tpageBBoxMapPDFFromTTempPageSelectPDF p = 
  let TLayerSelectInPage s others = glayers p 
      s' = tlayerBBoxFromTLayerSelect s
  in  GPage (gdimension p) (gbackground p) (gFromList (s':others))
      
      
ttempPageSelectPDFFromTPageBBoxMapPDF :: TPageBBoxMapPDF -> TTempPageSelectPDF 
ttempPageSelectPDFFromTPageBBoxMapPDF p = 
  let (x:xs) = gToList (glayers p)
      l = GLayer . TEitherAlterHitted . Left . gToList . gstrokes $ x
  in  GPage (gdimension p) (gbackground p) (TLayerSelectInPage l xs)
      
      