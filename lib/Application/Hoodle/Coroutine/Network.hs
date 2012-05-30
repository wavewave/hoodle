module Application.Hoodle.Coroutine.Network where

import Application.Hoodle.Type.XournalState
import Application.Hoodle.Type.Coroutine
import Application.Hoodle.Type.Event
import Application.Hoodle.ModelAction.Network
import Application.Hoodle.ModelAction.Select
import Application.Hoodle.Accessor

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
    
