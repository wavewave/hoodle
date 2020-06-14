{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | special actor for IO action
module Control.Monad.Trans.Crtn.IOActor where

import Control.Monad.Reader
import Control.Monad.Trans.Crtn
import Control.Monad.Trans.Crtn.Object

-- | first is
data IOOp e i o where
  DoIOAction :: IOOp e ((e -> IO ()) -> IO ()) (Either String ())

type IOActor e m r = SObjT (IOOp e) m r

-- |
doIOAction ::
  (Monad m) =>
  ((e -> IO ()) -> IO ()) ->
  CObjT (IOOp e) m (Either String ())
doIOAction act = do
  ans <- request (Arg DoIOAction act)
  case ans of
    Res DoIOAction r -> return r
    Ign -> return (Left "error in doing doIOAction")

-- |
ioactorgen :: (MonadIO m) => (e -> IO ()) -> SObjT (IOOp e) m ()
ioactorgen evhandler = ReaderT ioactorW
  where
    ioactorW (Arg DoIOAction act) = do
      liftIO (act evhandler)
      req <- request (Res DoIOAction (Right ()))
      ioactorW req
