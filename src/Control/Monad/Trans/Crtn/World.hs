{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables, KindSignatures #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.Trans.Crtn.Logger 
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- describe world object
--
-----------------------------------------------------------------------------

module Control.Monad.Trans.Crtn.World where 

import Control.Monad.Error 
-- 
import Control.Monad.Trans.Crtn
import Control.Monad.Trans.Crtn.Event 
import Control.Monad.Trans.Crtn.Logger 
import Control.Monad.Trans.Crtn.Object
-- 
import Prelude hiding ((.),id)


-- | 
data WorldOp m i o where 
  GiveEvent :: WorldOp m Event ()
  FlushLog :: WorldOp m (LogServer m ()) (LogServer m ())
  FlushQueue :: WorldOp m () [Either ActionOrder Event]

-- | 
type World m r = SObjT (WorldOp m) m r  


-- | 
giveEvent :: (Monad m) => Event -> CObjT (WorldOp m) m () 
giveEvent ev = request (Arg GiveEvent ev) >> return () 


-- | 
flushLog :: (Monad m) => LogServer m () -> CObjT (WorldOp m) m (LogServer m ()) 
flushLog logobj = do req <- request (Arg FlushLog logobj) 
                     case req of 
                       Res FlushLog logobj' -> return logobj' 
                       Ign -> return logobj 
                       _ -> error "error in flushLog"  -- allow partiality

-- | 
flushQueue :: (Monad m) => CObjT (WorldOp m) m [Either ActionOrder Event]
flushQueue = do req <- request (Arg FlushQueue ())
                case req of 
                  Res FlushQueue lst -> return lst 
                  Ign -> return [] 
                  _ -> error "error in flushQueue" -- allow partiality

