module Application.HXournal.GUI where

import Application.HXournal.Type 
import Application.HXournal.Coroutine
import Graphics.UI.Gtk hiding (get)
import Control.Monad.Coroutine.SuspensionFunctors
import Data.IORef

startGUI :: IORef (Await MyEvent (Iteratee MyEvent XournalStateIO ())) 
         -> IORef Int
         -> IO () 
startGUI tref sref = do 
  initGUI
  window <- windowNew 
  hbox <- hBoxNew False 0 
  vbox <- vBoxNew False 0 
  buttonleft    <- buttonNewWithLabel "<"
  buttonright   <- buttonNewWithLabel ">"
  buttonrefresh <- buttonNewWithLabel "Refresh"  
  buttonquit    <- buttonNewWithLabel "Quit"
  canvas <- drawingAreaNew
  set window [containerChild := vbox ]
  boxPackStart hbox buttonleft    PackGrow 0 
  boxPackStart hbox buttonright   PackGrow 0
  boxPackStart hbox buttonrefresh PackGrow 0
  boxPackStart hbox buttonquit    PackGrow 0 
  boxPackEnd vbox hbox   PackNatural 0 
  boxPackEnd vbox canvas PackGrow 0 
  canvas `on` sizeRequest $ return (Requisition 40 40)
 
  widgetShowAll window
  onClicked buttonleft    $ do putStrLn "<"
                               bouncecallback tref sref ButtonLeft
                               return ()
  onClicked buttonright   $ do putStrLn ">"
                               bouncecallback tref sref ButtonRight
                               return () 
  onClicked buttonrefresh $ do putStrLn "R"
                               bouncecallback tref sref ButtonRefresh
                               return ()
  onClicked buttonquit    $ do putStrLn "Q" 
                               bouncecallback tref sref ButtonQuit
                               mainQuit          
  onDestroy window mainQuit
  mainGUI 
  return ()

