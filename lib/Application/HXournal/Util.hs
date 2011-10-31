module Application.HXournal.Util where

waitUntil :: (Monad m) => (a -> Bool) -> m a -> m ()
waitUntil p act = do 
  a <- act
  if p a
    then return ()
    else waitUntil p act  
