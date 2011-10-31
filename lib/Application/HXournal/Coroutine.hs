{-# LANGUAGE FlexibleContexts #-}

module Application.HXournal.Coroutine where

--import Data.Functor.Identity (Identity(..))
import Control.Monad.Identity
import Control.Monad.Coroutine 
import Control.Monad.Trans
import Control.Monad.State
import Control.Concurrent hiding (yield)
import Control.Monad.Coroutine.SuspensionFunctors

import Data.IORef

import System.Random

import Application.HXournal.Type
import Application.HXournal.Util

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

check :: Iteratee Int MyStateIO Int 
check = do
  r1 <- await 
  liftIO (putStrLn ("yeah... I got r1 = " ++ show r1))
  return r1


bouncecallback :: IORef (Await MyEvent (Iteratee MyEvent MyStateIO ())) 
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


{-
rollDice :: IO Int 
rollDice = getStdRandom (randomR (1,6))

eventprocessor :: (Int -> IO ()) -> IO () 
eventprocessor callback = do 
  r <- rollDice
  putStrLn ("rollDice r = " ++ show r)
  callback r
  threadDelay 5000000
  eventprocessor callback 

-}

{-
atestcallback :: IO () 
atestcallback = do 
  putStrLn "I am event processor"
-}