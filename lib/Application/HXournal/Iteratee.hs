module Application.HXournal.Iteratee where 

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.IO.Class

import Application.HXournal.Type
import Application.HXournal.Util
import Application.HXournal.Draw
import Application.HXournal.Coroutine

import Text.Xournal.Type 

import Graphics.UI.Gtk hiding (get)

iter :: Iteratee MyEvent XournalStateIO () 
iter = do liftIO (putStrLn "I am waiting first result") 
          sequence_ (repeat eventProcess)
          return ()

changePage :: (Int -> Int) -> Iteratee MyEvent XournalStateIO () 
changePage modifyfn = do 
  xstate <- lift get 

  let totalnumofpages = (length . xoj_pages) (xoj xstate)
      oldpage = currpage xstate
  let newpage | modifyfn oldpage >= totalnumofpages = totalnumofpages - 1
              | modifyfn oldpage < 0  = 0 
              | otherwise = modifyfn oldpage 
  lift (put (xstate { currpage = newpage}))
  liftIO (updateCanvas <$> darea <*> xoj <*> return newpage $ xstate)
  liftIO . putStrLn $ "changing " ++ show oldpage ++ " to " ++ show newpage



eventProcess :: Iteratee MyEvent XournalStateIO ()
eventProcess = do 
  r1 <- await 
  case r1 of 
    ButtonLeft -> changePage (\x->x-1)
    ButtonRight -> changePage (+1)
    ButtonRefresh -> do 
      xstate <- lift get
      liftIO (updateCanvas <$> darea <*> xoj <*> currpage $ xstate)
      liftIO . putStrLn $ "refresh"
    ButtonQuit -> do  
      liftIO . putStrLn $ "quit"
    PenDown (x,y) -> do 
      liftIO . putStrLn $ "down " ++ show (x,y)
      canvas <- lift ( get >>= return . darea )
      sref <- lift (get >>= return . x_sref )
      tref <- lift (get >>= return . x_tref )
      liftIO (widgetAddEvents canvas [Button1MotionMask])
      connidmove <- liftIO (canvas `on` motionNotifyEvent $ tryEvent $ do 
                              (x,y) <- eventCoordinates
                              liftIO (bouncecallback tref sref (PenMove (x,y))))
      connidup   <- liftIO (canvas `on` buttonReleaseEvent $ tryEvent $ do 
                              (x,y) <- eventCoordinates
                              liftIO (bouncecallback tref sref (PenUp (x,y)) ))
      penProcess connidmove connidup
    _ -> defaultEventProcess r1


penProcess :: ConnectId DrawingArea -> ConnectId DrawingArea 
           -> Iteratee MyEvent XournalStateIO ()
penProcess connidmove connidup = do 
  r <- await 
  case r of 
    PenMove (x,y) -> do 
      liftIO . putStrLn $ "move " ++ show (x,y)
      penProcess connidmove connidup 
    PenUp (x,y) -> do 
      liftIO . putStrLn $ "up " ++ show (x,y)
      liftIO $ signalDisconnect connidmove
      liftIO $ signalDisconnect connidup
      return () 
    _ -> penProcess connidmove connidup




defaultEventProcess :: MyEvent -> Iteratee MyEvent XournalStateIO () 
defaultEventProcess UpdateCanvas = do 
  xstate <- lift get
  liftIO (updateCanvas <$> darea <*> xoj <*> currpage $ xstate )
defaultEventProcess _ = return ()
  
