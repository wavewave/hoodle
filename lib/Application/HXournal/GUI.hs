module Application.HXournal.GUI where

import Application.HXournal.Type 
import Application.HXournal.Coroutine
import Graphics.UI.Gtk hiding (get)
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.State
import Control.Monad.Coroutine

import Application.HXournal.Iteratee
import Application.HXournal.Draw
import Data.IORef

import Text.Xournal.Type
import Text.Xournal.Parse

-- startGUI :: IORef (Await MyEvent (Iteratee MyEvent XournalStateIO ())) 
--          -> IORef XournalState
--          -> IO () 
startGUI :: IO () 
-- startGUI tref sref = do 
startGUI = do 

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
  

  xojcontent <- read_xojgz "test.xoj" 
  let st = emptyXournalState { xoj = xojcontent, wdw = buttonrefresh, darea = canvas } 
  (r,st') <- runStateT (resume iter) st
  sref <- newIORef st'

  tref <- case r of 
            Left aw -> do 
              newIORef aw 
            Right _ -> error "what?"

  onExpose canvas $ const (bouncecallback tref sref UpdateCanvas >> return True)

-- const (updateCanvas canvas (xoj st) (currpage st) >> return True)

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

