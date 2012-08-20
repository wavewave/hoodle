module Event where 

-- | event 
data Event = Message String 
           | Open 
           | Close 
           | Render 
             deriving (Show,Eq)



