module Application.HXournal.Coroutine.Network where

import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Event
import Application.HXournal.ModelAction.Network

import Control.Monad.Trans

clipCopyToNetworkClipboard :: Iteratee MyEvent XournalStateIO ()
clipCopyToNetworkClipboard = do 
  liftIO $ putStrLn "clipCopyToNetworkClipboard called"
  liftIO $ copyContentsToNetworkClipboard 


clipPasteFromNetworkClipboard :: Iteratee MyEvent XournalStateIO ()
clipPasteFromNetworkClipboard = do 
  liftIO $ putStrLn "clipPasteFromNetworkClipboard called"
  liftIO $ getContentsFromNetworkClipboard


