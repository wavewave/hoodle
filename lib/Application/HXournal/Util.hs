module Application.HXournal.Util where

import Text.Xournal.Type

getLargestWidth :: Xournal -> Double 
getLargestWidth xoj = 
  let ws = map (dim_width . page_dim) (xoj_pages xoj)  
  in  maximum ws 

getLargestHeight :: Xournal -> Double 
getLargestHeight xoj = 
  let hs = map (dim_height . page_dim) (xoj_pages xoj)  
  in  maximum hs 





waitUntil :: (Monad m) => (a -> Bool) -> m a -> m ()
waitUntil p act = do 
  a <- act
  if p a
    then return ()
    else waitUntil p act  
