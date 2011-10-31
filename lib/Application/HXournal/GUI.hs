module Application.HXournal.GUI where

import Control.Monad.State
import Control.Monad.IO.Control

import Graphics.UI.Gtk hiding (get)

type MyStateIO = StateT Int IO 

startGUI :: IO () 
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
  onClicked buttonquit    mainQuit           
  widgetShowAll window
  onDestroy window mainQuit
  runStateT (sequence_ (repeat (foo' buttonleft buttonright buttonrefresh myaction))) 0  
  return () 

foo :: IO (MyStateIO ()) -> IO (MyStateIO ())
foo action = action 

foo' :: ( ButtonClass b1
        , ButtonClass b2
        , ButtonClass b3 ) 
       => b1 -> b2 -> b3 -> MyStateIO () -> MyStateIO () 
foo' w1 w2 w3 a = do
  
                     liftControlIO $ \run -> do 
                       onClicked w1 $ do putStrLn "<"
                                         run $ do { a ; a } 
                                         return ()
                       onClicked w2 $ do putStrLn ">"
                                         run a 
                                         return () 
                       onClicked w3 $ do putStrLn "R"
                                         run a
                                         return ()
                       mainIterationDo True
                       return ()


myaction :: MyStateIO () 
myaction = do 
  st <- get
  liftIO $ putStrLn ("myaction " ++ show st)
  put (st+1)

-- liftedmyaction = foo' myaction 

