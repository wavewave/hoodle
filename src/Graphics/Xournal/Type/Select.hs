module Graphics.Xournal.Type.Select where

import Text.Xournal.Type
import Graphics.Xournal.Type


type WholeOrPart a b = Either [a] (AlterList [a] b)

whole :: [a] -> WholeOrPart a b 
whole = Left 

part :: AlterList [a] b -> WholeOrPart a b
part = Right 

data XournalSelect = XournalSelect { pages :: WholeOrPart PageBBox PageSelect 
                                   }
--                     deriving Show 

data PageSelect = PageSelect { dimension :: Dimension
                             , background :: Background
                             , layers :: WholeOrPart LayerBBox LayerSelect 
                             } 
--                deriving Show

data LayerSelect = LayerSelect { strokes :: WholeOrPart StrokeBBox Hitted
                               } 
--                 deriving Show 

                         

