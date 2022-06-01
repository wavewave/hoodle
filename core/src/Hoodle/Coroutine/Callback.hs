module Hoodle.Coroutine.Callback where

import Control.Concurrent (MVar)
import Control.Exception
  ( ErrorCall,
    PatternMatchFail,
    SomeException (..),
    catch,
  )
import Control.Monad.Trans.Crtn.Driver (Driver)
import qualified Control.Monad.Trans.Crtn.EventHandler as E
import Hoodle.Util (errorlog)
import System.Exit (exitFailure)
import Prelude (IO, Maybe (..), show)

eventHandler :: MVar (Maybe (Driver e IO ())) -> e -> IO ()
eventHandler evar ev = E.eventHandler evar ev `catch` allexceptionproc

allexceptionproc :: SomeException -> IO ()
allexceptionproc e = do
  errorlog (show e)
  exitFailure

errorcall :: ErrorCall -> IO ()
errorcall e = errorlog (show e)

patternerr :: PatternMatchFail -> IO ()
patternerr e = errorlog (show e)
