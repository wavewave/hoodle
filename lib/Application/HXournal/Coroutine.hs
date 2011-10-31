{-# LANGUAGE FlexibleContexts #-}

module Application.HXournal.Coroutine where

import Control.Monad.Coroutine 
import Control.Monad.State
import Control.Monad.Coroutine.SuspensionFunctors
import Data.IORef
import Application.HXournal.Type

{-
pause :: Monad m => Trampoline m () 
pause = suspend (Identity $ return ())

run :: Monad m => Trampoline m x -> m x 
run t = resume t >>= either (run . runIdentity) return 

gen :: Generator Int IO Int   
gen = do lift (putStr "Yielding one, ") 
         yield 1
         lift (putStr "then two, ")
         yield 2 
         lift (putStr "return three: ") 
         return 3 

tram :: Trampoline IO ()
tram = do lift (putStr "Yielding one, ") 
          pause
          lift (putStr "then two, ")
          pause
          lift (putStr "return three: ") 
          pause 

check :: Iteratee Int XournalStateIO Int 
check = do
  r1 <- await 
  liftIO (putStrLn ("yeah... I got r1 = " ++ show r1))
  return r1
-}

bouncecallback :: IORef (Await MyEvent (Iteratee MyEvent XournalStateIO ())) 
               -> IORef Int 
               -> MyEvent 
               -> IO () 
bouncecallback tref sref input = do 
  Await cont <- readIORef tref 
  st <- readIORef sref
  (nr,st') <- runStateT (resume (cont input)) st 
  case nr of  
    Left  naw -> do writeIORef tref naw 
                    writeIORef sref st'
    Right val -> do putStrLn $ show val 
                    writeIORef tref (Await (\_ -> return ()))
                    writeIORef sref st'
  putStrLn "one step"
  return ()  

