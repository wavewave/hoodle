module Graphics.Xournal.Type.Select where

import Text.Xournal.Type
import Graphics.Xournal.Type
import Graphics.Xournal.Type.Map

import qualified Data.IntMap as M

-- type WholeOrPart a b = Either a (AlterList a b)

data XournalSelect = 
  XournalSelect 
  { pages :: Either (M.IntMap PageBBoxMap) (AlterList [PageBBox] [PageSelect])
  }

data PageSelect = 
  PageSelect 
  { dimension :: Dimension
  , background :: Background
  , layers :: Either (M.IntMap LayerBBox) (AlterList [LayerBBox] [LayerSelect]) 
  } 

data TempXournalSelect = 
  TempXournalSelect 
  { tx_pages :: M.IntMap PageBBoxMap 
  , tx_selectpage :: Maybe (Int, TempPageSelect)
  }

data TempPageSelect = 
  TempPageSelect 
  { tp_dim :: Dimension 
  , tp_bkg :: Background
  , tp_firstlayer :: LayerSelect
  , tp_otherlayer :: [LayerBBox] 
  }

data LayerSelect = 
  LayerSelect { strokes :: Either [StrokeBBox] (AlterList [StrokeBBox] Hitted)
              } 

xournalSelectFromXournalBBoxMap :: XournalBBoxMap -> XournalSelect 
xournalSelectFromXournalBBoxMap xoj = 
  XournalSelect { pages = Left (xbm_pages xoj) } 
  
tempXournalSelectFromXournalBBoxMap :: XournalBBoxMap -> TempXournalSelect 
tempXournalSelectFromXournalBBoxMap xoj = 
  TempXournalSelect { tx_pages = xbm_pages xoj 
                    , tx_selectpage = Nothing } 

pageBBoxMapFromTempPageSelect :: TempPageSelect -> PageBBoxMap
pageBBoxMapFromTempPageSelect (TempPageSelect dim bkg flayer olayers)
  = PageBBoxMap dim bkg lmap 
  where l = layerBBoxFromLayerSelect flayer 
        lmap = M.fromList $ zip [0..] (l:olayers)


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
