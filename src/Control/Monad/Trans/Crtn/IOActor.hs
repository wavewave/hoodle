{-# LANGUAGE GADTs, NoMonomorphismRestriction, ScopedTypeVariables #-}

------------------------------
-- | special actor for IO action 
------------------------------

module Control.Monad.Trans.Crtn.IOActor where

import Control.Monad.Reader
-- 
import Control.Monad.Trans.Crtn
import Control.Monad.Trans.Crtn.Event 
import Control.Monad.Trans.Crtn.Object 


-- | first is 
data IOOp i o where
  DoIOAction :: IOOp ((Event -> IO ()) -> IO ()) (Either String ())

type IOActor m r = SObjT IOOp m r

-- | 
doIOAction :: (Monad m) => ((Event -> IO ()) -> IO ()) 
           -> CObjT IOOp m (Either String ())
doIOAction act = do ans <- request (Arg DoIOAction act) 
                    case ans of 
                      Res DoIOAction r -> return r
                      Ign -> return (Left "error in doing doIOAction")

-- | 
ioactorgen :: (MonadIO m) => (Event -> IO ()) -> SObjT IOOp m ()
ioactorgen evhandler = ReaderT ioactorW 
  where ioactorW (Arg DoIOAction act) = do 
          liftIO (act evhandler) 
          req <- request (Res DoIOAction (Right ()))
          ioactorW req 


 


