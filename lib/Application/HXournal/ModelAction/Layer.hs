module Application.HXournal.ModelAction.Layer where

import Application.HXournal.Util

import Control.Compose
import Control.Category
import Data.Label
import Prelude hiding ((.),id)

import Data.Xournal.BBox
import Data.Xournal.Generic
import Data.Xournal.Buffer
import Data.Xournal.Select
import Graphics.Xournal.Render.BBoxMapPDF

import Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk as Gtk (get,set)

import Data.IORef

getCurrentLayerOrSet :: TPageBBoxMapPDFBuf -> (Maybe (TLayerBBoxBuf LyBuf),TPageBBoxMapPDFBuf)
getCurrentLayerOrSet pg = 
  let olayers = get g_layers pg
      nlayers = case olayers of 
                  NoSelect _ -> selectFirst olayers
                  Select _ -> olayers  
  in case nlayers of
      NoSelect _ -> (Nothing, set g_layers nlayers pg)
      Select osz -> (return . current =<< unO osz, set g_layers nlayers pg)


adjustCurrentLayer :: TLayerBBoxBuf LyBuf -> TPageBBoxMapPDFBuf -> TPageBBoxMapPDFBuf
adjustCurrentLayer nlayer pg = 
  let (molayer,pg') = getCurrentLayerOrSet pg
      layerzipper = pg'
  in maybe (set g_layers (Select .O . Just . singletonSZ $ nlayer) pg')
           (const $ let layerzipper = maybe (error "adjustCurrentLayer") id . unO . zipper . get g_layers $  pg'
                    in set g_layers (Select . O . Just . replace nlayer $ layerzipper) pg' )
           molayer 

layerChooseDialog :: IORef Int -> Int -> Int -> IO Dialog
layerChooseDialog layernumref cidx len = do 
    dialog <- dialogNew 
    layerentry <- entryNew
    entrySetText layerentry (show (succ cidx))
    label <- labelNew (Just (" / " ++ show len))
    -- button <- buttonNewWithLabel "test"
    hbox <- hBoxNew False 0 
    upper <- dialogGetUpper dialog
    boxPackStart upper hbox PackNatural 0 
    boxPackStart hbox layerentry PackNatural 0 
    boxPackStart hbox label PackGrow 0 
    widgetShowAll upper
    buttonOk <- dialogAddButton dialog stockOk ResponseOk
    buttonCancel <- dialogAddButton dialog stockCancel ResponseCancel

    buttonOk `on` buttonActivated $ do 
      txt <- Gtk.get layerentry entryText
      maybe (return ()) (modifyIORef layernumref . const . pred) . maybeRead $ txt
    return dialog


