module Control.Monad.Coroutine.Event where 

-- | event 
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

-- | action order 
data ActionOrder = ActionOrder ((Event -> IO ()) -> IO ())


