module Graphics.Xournal.Type.Select where

import Text.Xournal.Type
import Graphics.Xournal.Type
import Graphics.Xournal.Type.Map

import qualified Data.IntMap as M

-- type WholeOrPart a b = Either a (AlterList a b)

data XournalSelect = XournalSelect 
                     { pages :: Either (M.IntMap PageBBoxMap) (AlterList [PageBBox] [PageSelect])
                     }

data PageSelect = PageSelect 
                  { dimension :: Dimension
                  , background :: Background
                  , layers :: Either (M.IntMap LayerBBox) (AlterList [LayerBBox] [LayerSelect]) 
                  } 

data LayerSelect = LayerSelect { strokes :: Either [StrokeBBox] (AlterList [StrokeBBox] Hitted)
                               } 

xournalSelectFromXournalBBoxMap :: XournalBBoxMap -> XournalSelect 
xournalSelectFromXournalBBoxMap xoj = 
  XournalSelect { pages = Left (xbm_pages xoj) } 
  

layerBBoxFromLayerSelect :: LayerSelect -> LayerBBox 
layerBBoxFromLayerSelect layer = 
  let newstrs = case strokes layer of
                  Left strs -> strs                
                  Right alstrs -> concat $ interleave id unHitted $ alstrs
  in  LayerBBox newstrs 
      
pageBBoxMapFromPageSelect :: PageSelect -> PageBBoxMap
pageBBoxMapFromPageSelect page = 
  let newlyrs = case layers page of  
                  Left ls -> ls   
                  Right allyrs -> 
                    let layerlst = concat 
                                   . interleave id 
                                                (map layerBBoxFromLayerSelect) 
                                   $ allyrs 
                    in  M.fromList $ zip [0..] layerlst 
  in  PageBBoxMap { pbm_dim = dimension page  
                  , pbm_bkg = background page
                  , pbm_layers = newlyrs }
      
xournalBBoxMapFromXournalSelect :: XournalSelect -> XournalBBoxMap
xournalBBoxMapFromXournalSelect xoj = 
  let newpgs = case pages xoj of 
                 Left ps -> ps 
                 Right alpgs -> 
                   let pglst = concat 
                               . interleave (map mkPageBBoxMapFromPageBBox) 
                                            (map pageBBoxMapFromPageSelect) 
                               $ alpgs
                   in  M.fromList $ zip [0..] pglst 
  in  XournalBBoxMap newpgs

------------------
