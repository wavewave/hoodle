module Application.HXournal.ModelAction.Eraser where

import Control.Monad.State 
import Graphics.Xournal.Type 
import Graphics.Xournal.HitTest

eraseHitted :: AlterList NotHitted (AlterList NotHitted Hitted) 
               -> State (Maybe BBox) [StrokeBBox]
eraseHitted Empty = error "something wrong in eraseHitted"
eraseHitted (n :-Empty) = return (unNotHitted n)
eraseHitted (n:-h:-rest) = do 
  mid <- elimHitted h 
  return . (unNotHitted n ++) . (mid ++) =<< eraseHitted rest
