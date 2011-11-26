module Application.HXournal.Util.AlterList where
               
data AlterList a b = Empty | a :- AlterList b a
                   deriving (Show)

infixr 6 :-

fmapAL :: (a -> c) -> (b -> d) -> AlterList a b -> AlterList c d
fmapAL _ _ Empty = Empty 
fmapAL f g (x :- ys) = f x :- fmapAL g f ys 



