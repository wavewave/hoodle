module Event where

data Event = Message String 
           | Open 
           | Close 
           | Render 
           | Sound String 
           | Start
           | Finished 
           | Init Int 
           | Finish Int 
             deriving (Show,Eq) 
