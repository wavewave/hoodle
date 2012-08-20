module Event where 

-- | event 
data Event = Message String 
           | Open 
           | Close 
           | Render 
           | Sound String 
             deriving (Show,Eq)


data ActionOrder = ActionOrder (IO ())

