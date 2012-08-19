module Event where 

-- | event 
data Event = Message String 
           | Open 
           | Close 
             deriving (Show,Eq)



