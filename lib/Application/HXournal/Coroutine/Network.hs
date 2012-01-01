module Application.HXournal.Coroutine.Network where

import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Event
import Application.HXournal.ModelAction.Network
import Application.HXournal.Accessor

import Control.Monad.Trans

import Control.Category 
import Data.Label
import Prelude hiding ((.),id)

clipCopyToNetworkClipboard :: Iteratee MyEvent XournalStateIO ()
clipCopyToNetworkClipboard = do 
  liftIO $ putStrLn "clipCopyToNetworkClipboard called"
  xstate <- getSt
  let clip = get clipboard xstate 
  liftIO $ copyContentsToNetworkClipboard clip 


clipPasteFromNetworkClipboard :: Iteratee MyEvent XournalStateIO ()
clipPasteFromNetworkClipboard = do 
  xstate <- getSt 
  liftIO $ putStrLn "clipPasteFromNetworkClipboard called"
  mclip <- liftIO $ getContentsFromNetworkClipboard
  maybe (return ()) (\clip -> putSt . set clipboard clip $ xstate) mclip 

