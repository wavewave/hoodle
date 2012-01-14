module Application.HXournal.Coroutine.Network where

import Application.HXournal.Type.XournalState
import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Event
import Application.HXournal.ModelAction.Network
import Application.HXournal.ModelAction.Select
import Application.HXournal.Accessor

import Control.Monad.Trans

import Control.Category 
import Data.Label
import Prelude hiding ((.),id)

clipCopyToNetworkClipboard :: MainCoroutine () -- Iteratee MyEvent XournalStateIO ()
clipCopyToNetworkClipboard = do 
  liftIO $ putStrLn "clipCopyToNetworkClipboard called"
  xstate <- getSt
  let mncconf = get networkClipboardInfo xstate
  flip (maybe (return ())) mncconf $ \ncconf -> do 
    let clip = get clipboard xstate 
    liftIO $ copyContentsToNetworkClipboard ncconf clip 


clipPasteFromNetworkClipboard :: MainCoroutine () -- Iteratee MyEvent XournalStateIO ()
clipPasteFromNetworkClipboard = do 
  xstate <- getSt 
  let ui = get gtkUIManager xstate 
  let mncconf = get networkClipboardInfo xstate
  flip (maybe (return ())) mncconf $ \ncconf -> do 
    mclip <- liftIO $ getContentsFromNetworkClipboard ncconf
    flip (maybe (return ())) mclip $ \clip -> do 
      putSt . set clipboard clip $ xstate
      liftIO $ togglePaste ui True
    
