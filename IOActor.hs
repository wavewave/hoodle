{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables #-}

------------------------------
-- | special actor for IO action 
------------------------------

module IOActor where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans 
-- 
import Coroutine
import Event 
import Object 


-- | 
data IOOp i o where
  DoIOAction :: IOOp (IO ()) (Either String ())

-- | 
doIOAction :: (Monad m) => IO () -> ClientObj IOOp m (Either String ())
doIOAction act = do ans <- request (Input DoIOAction act) 
                    case ans of 
                      Output DoIOAction r -> return r
                      Ignore -> return (Left "error in doing doIOAction")

-- | 
ioactor :: (MonadIO m) => ServerObj IOOp m ()
ioactor = ReaderT ioactorW 
  where ioactorW (Input DoIOAction act) = do 
          liftIO act 
          req <- request (Output DoIOAction (Right ()))
          ioactorW req 


 


