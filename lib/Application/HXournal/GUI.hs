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

startGUI :: FilePath -> IO () 
startGUI fname = do 

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
  canvas `on` sizeRequest $ return (Requisition 480 640)
 
  widgetShowAll window
  

  xojcontent <- read_xojgz fname 
  let st = emptyXournalState { xoj = xojcontent, wdw = buttonrefresh, darea = canvas } 
  (r,st') <- runStateT (resume iter) st
  sref <- newIORef st'

  tref <- case r of 
            Left aw -> do 
              newIORef aw 
            Right _ -> error "what?"

  writeIORef sref st' {callback = bouncecallback tref sref }

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
  canvas `on` buttonPressEvent $ tryEvent $ do 
    (x,y) <- eventCoordinates
    liftIO (bouncecallback tref sref (PenDown (x,y)) )
  
  canvas `on` buttonReleaseEvent $ tryEvent $ do 
    (x,y) <- eventCoordinates
    liftIO (bouncecallback tref sref (PenUp (x,y)) )

  widgetAddEvents canvas [Button1MotionMask]


  onDestroy window mainQuit
  mainGUI 
  return ()

