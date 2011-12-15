module Graphics.Xournal.Type.BBoxMapPDF where

import Graphics.Xournal.Type 
import Graphics.Xournal.Type.Generic
import Data.IntMap

import Data.ByteString hiding (putStrLn, empty)
import qualified Data.ByteString.Char8 as C hiding (empty)
import qualified Graphics.UI.Gtk.Poppler.Document as Poppler

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

data BkgPDFOption = DrawBkgPDF | DrawWhite

type TPageBBoxMapPDF = TPageBBoxMapBkg BackgroundPDFDrawable 

type TXournalBBoxMapPDF = TXournalBBoxMapBkg BackgroundPDFDrawable

type TTempPageSelectPDF = GPage BackgroundPDFDrawable (TLayerSelectInPage []) TLayerBBox

type TTempXournalSelectPDF = GSelect (IntMap TPageBBoxMapPDF) (Maybe (Int, TTempPageSelectPDF))


emptyTXournalBBoxMapPDF :: TXournalBBoxMapPDF
emptyTXournalBBoxMapPDF = GXournal empty


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