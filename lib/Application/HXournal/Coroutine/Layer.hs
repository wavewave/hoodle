module Application.HXournal.Coroutine.Layer where

import Application.HXournal.Type.Coroutine
import Control.Monad.Trans

makeNewLayer :: MainCoroutine () 
makeNewLayer = do 
  liftIO $ putStrLn "makeNewLayer called"

gotoNextLayer :: MainCoroutine ()
gotoNextLayer = do 
  liftIO $ putStrLn "gotoNextLayer called"

gotoPrevLayer :: MainCoroutine ()
gotoPrevLayer = do 
  liftIO $ putStrLn "gotoPrevLayer called"


deleteCurrentLayer :: MainCoroutine ()
deleteCurrentLayer = do 
  liftIO $ putStrLn "deleteCurrentLayer called"