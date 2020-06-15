module Control.Monad.Trans.Crtn.Event where

{-
import Data.ByteString
import Data.UUID
import Data.SafeCopy

class (Show e,Eq e, SafeCopy e) => Eventable e where
  eventClassID :: e -> UUID
  eventWrap :: e -> Event

-- | event
data Event = Event (UUID,ByteString)
           deriving (Show,Eq)


{-
data Event = Message String
           | Open
           | Close
           | Render
           | Sound String
           | Start
           | Finished
           | Init Int
           | Finish Int
             deriving (Show,Eq) -}
-}

-- | action order
data ActionOrder e = ActionOrder ((e -> IO ()) -> IO e)

-- | event or action
type EvOrAct e = Either (ActionOrder e) e
