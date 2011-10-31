module Application.HXournal.Iteratee where 

import Control.Monad
import Control.Monad.State
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.IO.Class

import Application.HXournal.Type
import Application.HXournal.Util
import Application.HXournal.Draw

import Text.Xournal.Type 

import Graphics.UI.Gtk hiding (get)

iter :: Iteratee MyEvent XournalStateIO () 
iter = do liftIO (putStrLn "I am waiting first result") 
          sequence_ (repeat eventProcess)
          return ()

changePage :: (Int -> Int) -> Iteratee MyEvent XournalStateIO () 
changePage modifyfn = do 
  XournalState xj wdw da oldpage <- lift get 

  let totalnumofpages = (length . xoj_pages) xj
      
  let newpage | modifyfn oldpage >= totalnumofpages = totalnumofpages - 1
              | modifyfn oldpage < 0  = 0 
              | otherwise = modifyfn oldpage 
  lift (put (XournalState xj wdw da newpage))
  liftIO (updateCanvas da xj newpage) 
  liftIO . putStrLn $ "changing " ++ show oldpage ++ " to " ++ show newpage



eventProcess :: Iteratee MyEvent XournalStateIO ()
eventProcess = do 
  r1 <- await 
  case r1 of 
    ButtonLeft -> changePage (\x->x-1)
    ButtonRight -> changePage (+1)
    ButtonRefresh -> do 
      XournalState xj wdw da st <- lift get
      liftIO (updateCanvas da xj st)
      liftIO . putStrLn $ "refresh"
    ButtonQuit -> do  
      liftIO . putStrLn $ "quit"
    _ -> defaultEventProcess r1


defaultEventProcess :: MyEvent -> Iteratee MyEvent XournalStateIO () 
defaultEventProcess UpdateCanvas = do 
  XournalState xj wdw da st <- lift get
  liftIO (updateCanvas da xj st)
defaultEventProcess _ = return ()
  
