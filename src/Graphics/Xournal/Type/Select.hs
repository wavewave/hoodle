module Graphics.Xournal.Type.Select where

import Text.Xournal.Type
import Graphics.Xournal.Type

import qualified Data.IntMap as M

type WholeOrPart a b = Either a (AlterList a b)

data XournalSelect = XournalSelect 
                     { pages :: WholeOrPart [PageBBox] [PageSelect]
                     }

data PageSelect = PageSelect 
                  { dimension :: Dimension
                  , background :: Background
                  , layers :: WholeOrPart [LayerBBox] [LayerSelect] 
                  } 

data LayerSelect = LayerSelect { strokes :: WholeOrPart [StrokeBBox] Hitted
                               } 

xournalSelectFromXournalBBox :: XournalBBox -> XournalSelect 
xournalSelectFromXournalBBox xoj = 
  XournalSelect { pages = Left (xournalPages xoj) } 
  

layerBBoxFromLayerSelect :: LayerSelect -> LayerBBox 
layerBBoxFromLayerSelect layer = 
  let newstrs = case strokes layer of
                  Left strs -> strs                
                  Right alstrs -> concat $ interleave id unHitted $ alstrs
  in  LayerBBox newstrs 
      
pageBBoxFromPageSelect :: PageSelect -> PageBBox      
pageBBoxFromPageSelect page = 
  let newlyrs = case layers page of  
                  Left ls -> ls   
                  Right allyrs -> 
                    concat $ interleave id (map layerBBoxFromLayerSelect) 
                           $ allyrs 
  in  PageBBox { pagebbox_dim = dimension page  
               , pagebbox_bkg = background page
               , pagebbox_layers = newlyrs }
      

xournalBBoxFromXournalSelect :: XournalSelect -> XournalBBox 
xournalBBoxFromXournalSelect xoj = 
  let newpgs = case pages xoj of 
                 Left ps -> ps 
                 Right alpgs -> 
                   concat $ interleave id (map pageBBoxFromPageSelect) 
                          $ alpgs
  in  XournalBBox newpgs

------------------
