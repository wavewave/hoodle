module Hoodle.Coroutine.Network where

import Hoodle.Type.XournalState
import Hoodle.Type.Coroutine
import Hoodle.Type.Event
import Hoodle.ModelAction.Network
import Hoodle.ModelAction.Select
import Hoodle.Accessor

import Control.Monad.Trans

import Control.Category 
import Data.Label
import Prelude hiding ((.),id)

clipCopyToNetworkClipboard :: MainCoroutine () -- Iteratee UserEvent XournalStateIO ()
clipCopyToNetworkClipboard = do 
  liftIO $ putStrLn "clipCopyToNetworkClipboard called"
  xstate <- getSt
  let mncconf = get networkClipboardInfo xstate
  flip (maybe (return ())) mncconf $ \ncconf -> do 
    let clip = get clipboard xstate 
    liftIO $ copyContentsToNetworkClipboard ncconf clip 


clipPasteFromNetworkClipboard :: MainCoroutine () -- Iteratee UserEvent XournalStateIO ()
clipPasteFromNetworkClipboard = do 
  xstate <- getSt 
  let ui = get gtkUIManager xstate 
  let mncconf = get networkClipboardInfo xstate
  flip (maybe (return ())) mncconf $ \ncconf -> do 
    mclip <- liftIO $ getContentsFromNetworkClipboard ncconf
    flip (maybe (return ())) mclip $ \clip -> do 
      putSt . set clipboard clip $ xstate
      liftIO $ togglePaste ui True
    
