module Control.Monad.Coroutine.Event where 

-- | event 
data Event = Message String 
           | Open 
           | Close 
           | Render 
           | Sound String 
             deriving (Show,Eq)

-- | action order 
data ActionOrder = ActionOrder ((Event -> IO ()) -> IO ())


