{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Coroutine where

import Control.Concurrent.MVar (MVar)
import Control.Monad (liftM, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Crtn ((<==|), CrtnT, request)
import qualified Control.Monad.Trans.Crtn.Driver as D
  ( Driver,
    DrvOp,
  )
import Control.Monad.Trans.Crtn.Logger
  ( LogInput,
    LogOp (..),
    LogServer,
    MonadLog (..),
    writeLog,
  )
import Control.Monad.Trans.Crtn.Object (Arg (..), CObjT, EStT, Res (..), SObjBT, SObjT)
import Control.Monad.Trans.Crtn.World (WorldOp (..))
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State (StateT (..))
import Event (AllEvent (..))
import State (HoodleState)
import System.IO (hFlush, hPutStrLn, stdout)

putStrLnAndFlush :: String -> IO ()
putStrLnAndFlush s = do
  hPutStrLn stdout s
  hFlush stdout

data MainOp i o where
  DoEvent :: MainOp AllEvent ()

doEvent :: (Monad m) => AllEvent -> CObjT MainOp m ()
doEvent ev = request (Arg DoEvent ev) >> pure ()

type MainCoroutine = MainObjB

type MainObjB = SObjBT MainOp (EStT HoodleState WorldObjB)

type MainObj = SObjT MainOp (EStT HoodleState WorldObjB)

type WorldObj = SObjT (WorldOp AllEvent DriverB) DriverB

type WorldObjB = SObjBT (WorldOp AllEvent DriverB) DriverB

type Driver a = D.Driver AllEvent IO a

type DriverB = SObjBT (D.DrvOp AllEvent) IO

type EventVar = MVar (Maybe (Driver ()))

simplelogger :: (MonadLog m) => LogServer m ()
simplelogger = loggerW 0

-- |
loggerW :: forall m. (MonadLog m) => Int -> LogServer m ()
loggerW num = ReaderT (f num)
  where
    f :: Int -> LogInput -> CrtnT (Res LogOp) (Arg LogOp) m ()
    f n (Arg WriteLog msg) = do
      lift (scribe ("log number " ++ show n ++ " : " ++ msg))
      req' <- request (Res WriteLog ())
      f (n + 1) req'

errorlog :: String -> IO ()
errorlog = putStrLnAndFlush

-- |
world :: HoodleState -> MainObj () -> WorldObj ()
world xstate initmc = ReaderT staction
  where
    staction req = void $ runStateT erract xstate
      where
        erract = do
          r <- runExceptT (go initmc req)
          case r of
            Left e -> liftIO (errorlog (show e))
            Right _ -> pure ()
    go ::
      MainObj () ->
      Arg (WorldOp AllEvent DriverB) ->
      EStT HoodleState WorldObjB ()
    go mcobj (Arg GiveEvent ev) = do
      Right mcobj' <- liftM (fmap fst) (mcobj <==| doEvent ev)
      req <- lift $ lift $ request $ Res GiveEvent ()
      go mcobj' req
    go mcobj (Arg FlushLog logobj) = do
      -- -- disable log
      -- Right logobj' <- lift $ lift $ lift $ liftM (fmap fst) (logobj <==| writeLog ("[Log]"))
      let logobj' = logobj
      req <- lift $ lift $ request $ Res FlushLog logobj'
      go mcobj req
    go mcobj (Arg FlushQueue ()) = do
      req <- lift $ lift $ request $ Res FlushQueue []
      go mcobj req
