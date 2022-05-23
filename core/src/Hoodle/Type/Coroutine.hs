{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hoodle.Type.Coroutine where

import Control.Applicative
import Control.Concurrent
import Control.Error.Util (hoistEither)
import Control.Lens ((%~), (.~), (^.))
import Control.Monad (void)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Crtn
import qualified Control.Monad.Trans.Crtn.Driver as D
import Control.Monad.Trans.Crtn.Logger
import Control.Monad.Trans.Crtn.Object
import Control.Monad.Trans.Crtn.Queue
import Control.Monad.Trans.Crtn.World
import Control.Monad.Trans.Except (runExceptT)
import Hoodle.Type.Event
import Hoodle.Type.HoodleState
import Hoodle.Util

-- |
data MainOp i o where
  DoEvent :: MainOp AllEvent ()

doEvent :: (Monad m) => AllEvent -> CObjT MainOp m ()
doEvent ev = void $ request (Arg DoEvent ev)

-- |
type MainCoroutine = MainObjB

type MainObjB = SObjBT MainOp (EStT HoodleState WorldObjB)

-- |
type MainObj = SObjT MainOp (EStT HoodleState WorldObjB)

-- |
type WorldObj = SObjT (WorldOp AllEvent DriverB) DriverB

-- |
type WorldObjB = SObjBT (WorldOp AllEvent DriverB) DriverB

-- |
world :: HoodleState -> MainObj () -> WorldObj ()
world xstate initmc = ReaderT staction
  where
    staction req = void (runStateT erract xstate)
      where
        erract = do
          r <- runExceptT (go initmc req)
          case r of
            Left e -> liftIO (errorlog (show e))
            Right _r' -> return ()
    go ::
      MainObj () ->
      Arg (WorldOp AllEvent DriverB) ->
      EStT HoodleState WorldObjB ()
    go mcobj (Arg GiveEvent ev) = do
      Right mcobj' <- fmap (fmap fst) (mcobj <==| doEvent ev)
      req <- lift . lift $ request (Res GiveEvent ())
      go mcobj' req
    go mcobj (Arg FlushLog logobj) = do
      logf <- gets (^. tempLog)
      let msg = logf ""
      if (not . null) msg
        then do
          Right logobj' <- lift . lift . lift $ fmap (fmap fst) (logobj <==| writeLog msg)
          modify (tempLog .~ id)
          req <- lift . lift $ request (Res FlushLog logobj')
          go mcobj req
        else do
          req <- lift . lift $ request Ign
          go mcobj req
    go mcobj (Arg FlushQueue ()) = do
      q <- gets (^. tempQueue)
      let lst = fqueue q ++ reverse (bqueue q)
      modify (tempQueue .~ emptyQueue)
      req <- lift . lift $ request (Res FlushQueue lst)
      go mcobj req

-- |
type Driver a = D.Driver AllEvent IO a -- SObjT MainOp IO a

-- |
type DriverB = SObjBT (D.DrvOp AllEvent) IO

-- |
type EventVar = MVar (Maybe (Driver ()))

-- |
maybeError :: String -> Maybe a -> MainCoroutine a
maybeError str = maybe (lift . hoistEither . Left . Other $ str) return

-- |
doIOaction :: ((AllEvent -> IO ()) -> IO AllEvent) -> MainCoroutine ()
doIOaction action = modify (tempQueue %~ enqueue (mkIOaction action))
