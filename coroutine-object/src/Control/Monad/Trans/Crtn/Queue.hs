{-# LANGUAGE StandaloneDeriving #-}

module Control.Monad.Trans.Crtn.Queue where 

---------------------------
-- queue
---------------------------


data Queue a = Queue { fqueue :: [a] 
                     , bqueue :: [a] } 

deriving instance (Show a) => Show (Queue a)

emptyQueue :: Queue a 
emptyQueue = Queue [] []

enqueue :: a -> Queue a -> Queue a 
enqueue y (Queue xs ys) = Queue xs (y:ys) 

dequeue :: Queue a -> (Queue a, Maybe a) 
dequeue (Queue (x:xs) ys) = (Queue xs ys, Just x)
dequeue q@(Queue [] []) = (q,Nothing)
dequeue (Queue [] ys) = dequeue (Queue (reverse ys) [])

