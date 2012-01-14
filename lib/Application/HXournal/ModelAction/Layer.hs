module Application.HXournal.ModelAction.Layer where

import Control.Compose
import Control.Category
import Data.Label
import Prelude hiding ((.),id)

import Data.Xournal.BBox
import Data.Xournal.Generic
import Data.Xournal.Buffer
import Data.Xournal.Select
import Graphics.Xournal.Render.BBoxMapPDF

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

