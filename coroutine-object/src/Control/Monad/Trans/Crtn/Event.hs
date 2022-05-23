module Control.Monad.Trans.Crtn.Event where

-- | action order
newtype ActionOrder e = ActionOrder ((e -> IO ()) -> IO e)

-- | event or action
type EvOrAct e = Either (ActionOrder e) e
