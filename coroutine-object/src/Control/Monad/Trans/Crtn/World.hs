{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Monad.Trans.Crtn.World where

import Control.Monad.Trans.Crtn
import Control.Monad.Trans.Crtn.Event
import Control.Monad.Trans.Crtn.Logger
import Control.Monad.Trans.Crtn.Object
import Prelude hiding ((.), id)

-- |
data WorldOp e m i o where
  GiveEvent :: WorldOp e m e ()
  FlushLog :: WorldOp e m (LogServer m ()) (LogServer m ())
  FlushQueue :: WorldOp e m () [Either (ActionOrder e) e]

-- |
type World e m r = SObjT (WorldOp e m) m r

-- |
giveEvent :: (Monad m) => e -> CObjT (WorldOp e m) m ()
giveEvent ev = request (Arg GiveEvent ev) >> return ()

-- |
flushLog :: (Monad m) => LogServer m () -> CObjT (WorldOp e m) m (LogServer m ())
flushLog logobj = do
  req <- request (Arg FlushLog logobj)
  case req of
    Res FlushLog logobj' -> return logobj'
    Ign -> return logobj
    _ -> error "error in flushLog" -- allow partiality

-- |
flushQueue :: (Monad m) => CObjT (WorldOp e m) m [Either (ActionOrder e) e]
flushQueue = do
  req <- request (Arg FlushQueue ())
  case req of
    Res FlushQueue lst -> return lst
    Ign -> return []
    _ -> error "error in flushQueue" -- allow partiality
