{-# LANGUAGE FlexibleContexts #-}

module Application.HXournal.Coroutine where

import Data.Functor.Identity (Identity(..))
import Control.Monad.Coroutine 
import Control.Monad.Trans
import Control.Concurrent hiding (yield)

import Data.IORef

type Trampoline m x = Coroutine Identity m x 
type Generator a m x = Coroutine ((,) a) m x

pause :: Monad m => Trampoline m () 
pause = suspend (Identity $ return ())

yield :: (Monad m, Functor ((,) x)) => x -> Generator x m () 
yield x = suspend (x, return ())

run :: Monad m => Trampoline m x -> m x 
run t = resume t >>= either (run . runIdentity) return 

runGenerator :: Monad m => Generator x m r -> m ([x],r)
runGenerator = run' id where
  run' f g = resume g
    >>= either (\(x,cont) -> run' (f . (x:)) cont)
               (\r -> return (f [], r))

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


atestcallback :: IO () 
atestcallback = do 
  putStrLn "I am event processor"


bouncecallback :: IORef (Trampoline IO ()) ->  IO () 
bouncecallback tref = do 
  tr <- readIORef tref 
  r <- resume tr
  case r of  
    Left tr' -> writeIORef tref (runIdentity tr') 
    Right val -> putStrLn $ show val 
  putStrLn "one step"
  return ()  


{-
bouncecallback :: IORef (Generator Int IO Int) ->  IO () 
bouncecallback gref = do 
  gen <- readIORef gref 
  r <- resume gen
  case r of  
    Left (n,gen') -> writeIORef gref gen' 
    Right val -> putStrLn $ show val 
  putStrLn "one step"
  return ()  
-}

{-
bouncecallback :: IORef (Coroutine s m x) ->  IO () 
bouncecallback gref = do 
  gen <- readIORef gref 
  r <- resume gen
  case r of  
    Left (n,gen') -> writeIORef gref gen' 
    Right val -> putStrLn $ show val 
  putStrLn "one step"
  return ()  
-}


eventprocessor :: IO () -> IO () 
eventprocessor callback = do 
  -- putStrLn "I am event processor" 
  threadDelay 5000000
  callback 
  eventprocessor callback 
