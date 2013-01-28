{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Coroutine 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Type.Coroutine where

-- from other packages 
import           Control.Applicative
import           Control.Concurrent
import           Control.Lens 
-- import           Control.Monad.Error
import           Control.Monad.Reader 
import           Control.Monad.State
import           Control.Monad.Trans.Either 
-- import           Data.IORef 
-- from hoodle-platform
import           Control.Monad.Trans.Crtn 
import           Control.Monad.Trans.Crtn.Object
import qualified Control.Monad.Trans.Crtn.Driver as D
import           Control.Monad.Trans.Crtn.Logger 
import           Control.Monad.Trans.Crtn.Queue 
import           Control.Monad.Trans.Crtn.World
-- from this package
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState 
import           Hoodle.Util
-- 

-- |
data MainOp i o where 
  DoEvent :: MainOp MyEvent () 

doEvent :: (Monad m) => MyEvent -> CObjT MainOp m () 
doEvent ev = request (Arg DoEvent ev) >> return ()

{-
instance (Monad m) => MonadState HoodleState (EStT HoodleState m) where
  get = lift get
  put = lift . put 
-}

-- |
type MainCoroutine = MainObjB 
                     
type MainObjB = SObjBT MainOp (EStT HoodleState WorldObjB)

-- | 
type MainObj = SObjT MainOp (EStT HoodleState WorldObjB)

-- | 
nextevent :: MainCoroutine MyEvent 
nextevent = do Arg DoEvent ev <- request (Res DoEvent ())
               return ev 

-- | 
type WorldObj = SObjT (WorldOp MyEvent DriverB) DriverB  

-- | 
type WorldObjB = SObjBT (WorldOp MyEvent DriverB) DriverB 

-- | 
world :: HoodleState -> MainObj () -> WorldObj ()
-- world :: HoodleState -> (ReaderT (Arg MainOp) MainCoroutine ()) -> WorldObj ()
world xstate initmc = ReaderT staction  
  where 
    staction req = runStateT erract xstate >> return ()
      -- where erract = go initmc req 
      where erract = do r <- runEitherT (go initmc req) 
                        case r of 
                          Left e -> liftIO (errorlog (show e)) 
                          Right _r' -> return () --  return r' 
    go :: MainObj() 
          -> Arg (WorldOp MyEvent DriverB) 
          -> EStT HoodleState WorldObjB () 
    go mcobj (Arg GiveEvent ev) = do 
      Right mcobj' <- liftM (fmap fst) (mcobj <==| doEvent ev)
      req <- lift . lift $ request (Res GiveEvent ())
      go mcobj' req  
    go mcobj (Arg FlushLog logobj) = do  
      logf <- (^. tempLog) <$> get  
      let msg = logf "" 
      if ((not.null) msg)
        then do 
          Right logobj' <- lift . lift . lift $ liftM (fmap fst) (logobj <==| writeLog msg)
          modify (tempLog .~ id)
          req <- lift . lift $ request (Res FlushLog logobj')
          go mcobj req 
        else do 
          req <- lift . lift $ request Ign 
          go mcobj req 
    go mcobj (Arg FlushQueue ()) = do 
      q <- (^. tempQueue) <$> get 
      let lst = fqueue q ++ reverse (bqueue q)
      modify (tempQueue .~ emptyQueue)
      req <- lift .  lift $ request (Res FlushQueue lst)
      go mcobj req 




-- | 
type Driver a = D.Driver MyEvent IO a -- SObjT MainOp IO a 

-- | 
type DriverB = SObjBT (D.DrvOp MyEvent) IO  

-- | 
type EventVar = MVar (Maybe (Driver ()))




-- | 
maybeError :: String -> Maybe a -> MainCoroutine a
maybeError str = maybe (lift . hoistEither . Left . Other $ str) return 



