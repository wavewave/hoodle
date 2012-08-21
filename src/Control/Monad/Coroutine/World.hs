{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables, KindSignatures #-}

----------------------------
-- | describe world object
----------------------------

module Control.Monad.Coroutine.World where 

import Control.Monad.Error 
-- 
import Control.Monad.Coroutine
import Control.Monad.Coroutine.Event 
import Control.Monad.Coroutine.Logger 
import Control.Monad.Coroutine.Object
-- 
import Prelude hiding ((.),id)


-- | 
data WorldOp m i o where 
  GiveEvent :: WorldOp m Event ()
  FlushLog :: WorldOp m (LogServer m ()) (LogServer m ())
  FlushQueue :: WorldOp m () [Either ActionOrder Event]

-- | 
type World m r = ServerObj (WorldOp m) m r  


-- | 
giveEvent :: (Monad m) => Event -> ClientObj (WorldOp m) m () 
giveEvent ev = request (Input GiveEvent ev) >> return () 


-- | 
flushLog :: (Monad m) => LogServer m () -> ClientObj (WorldOp m) m (LogServer m ()) 
flushLog logobj = do req <- request (Input FlushLog logobj) 
                     case req of 
                       Output FlushLog logobj' -> return logobj' 
                       Ignore -> return logobj 
                       _ -> error "error in flushLog"  -- allow partiality

-- | 
flushQueue :: (Monad m) => ClientObj (WorldOp m) m [Either ActionOrder Event]
flushQueue = do req <- request (Input FlushQueue ())
                case req of 
                  Output FlushQueue lst -> return lst 
                  Ignore -> return [] 
                  _ -> error "error in flushQueue" -- allow partiality

