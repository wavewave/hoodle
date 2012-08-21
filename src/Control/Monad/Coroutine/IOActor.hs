{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables #-}

------------------------------
-- | special actor for IO action 
------------------------------

module Control.Monad.Coroutine.IOActor where

import Control.Monad.Reader
-- 
import Control.Monad.Coroutine
import Control.Monad.Coroutine.Event 
import Control.Monad.Coroutine.Object 


-- | first is 
data IOOp i o where
  DoIOAction :: IOOp ((Event -> IO ()) -> IO ()) (Either String ())

type IOActor m r = ServerObj IOOp m r

-- | 
doIOAction :: (Monad m) => ((Event -> IO ()) -> IO ()) 
           -> ClientObj IOOp m (Either String ())
doIOAction act = do ans <- request (Input DoIOAction act) 
                    case ans of 
                      Output DoIOAction r -> return r
                      Ignore -> return (Left "error in doing doIOAction")

-- | 
ioactorgen :: (MonadIO m) => (Event -> IO ()) -> ServerObj IOOp m ()
ioactorgen evhandler = ReaderT ioactorW 
  where ioactorW (Input DoIOAction act) = do 
          liftIO (act evhandler) 
          req <- request (Output DoIOAction (Right ()))
          ioactorW req 


 


