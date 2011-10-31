{-# LANGUAGE FlexibleContexts #-}

module Application.HXournal.Coroutine where

import Data.Functor.Identity (Identity(..))
import Control.Monad.Coroutine 
import Control.Monad.Trans
import Control.Concurrent hiding (yield)
import Control.Monad.Coroutine.SuspensionFunctors

import Data.IORef

import System.Random


type Trampoline m x = Coroutine Identity m x 
type Generator a m x = Coroutine (Yield a) m x
type Iteratee a m x = Coroutine (Await a) m x

pause :: Monad m => Trampoline m () 
pause = suspend (Identity $ return ())

{-
yield :: (Monad m, Functor ((,) x)) => x -> Generator x m () 
yield x = suspend (x, return ())

await :: (Monad m, Functor ((,) x)) => Iteratee x m x 
await = suspend return 
-}


run :: Monad m => Trampoline m x -> m x 
run t = resume t >>= either (run . runIdentity) return 

{-
runGenerator :: Monad m => Generator x m r -> m ([x],r)
runGenerator = run' id where
  run' f g = resume g
    >>= either (\(x,cont) -> run' (f . (x:)) cont)
               (\r -> return (f [], r))

runIteratee :: Monad m => [x] -> Iteratee x m r -> m r 
runIteratee (x:rest) i = 
  resume i >>= either (\cont -> runIteratee rest (cont x)) return 
runIteratee [] i = 
  resume i 
    >>= either (\cont -> runIteratee [] (cont $ error "No more values to feed."))
               return 

-}

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

{-
newtram :: Coroutine (Request Int ()) Trampoline IO ()
newtram = do lift (putStr "Yielding one, ") 
             pause
             lift (putStr "then two, ")
             pause
             lift (putStr "return three: ") 
             pause 
-}

iter :: Iteratee Int IO () 
iter = do lift (putStrLn "I am waiting first result") 
          waitUntil (==5) check 
          r2 <- await 
          lift (putStrLn ("yeah... I got r2 = " ++ show r2))
          return ()


check :: Iteratee Int IO Int 
check = do
  r1 <- await 
  lift (putStrLn ("yeah... I got r1 = " ++ show r1))
  return r1


waitUntil :: (Monad m) => (a -> Bool) -> m a -> m ()
waitUntil pred act = do 
  a <- act
  if pred a
    then return ()
    else waitUntil pred act  


atestcallback :: IO () 
atestcallback = do 
  putStrLn "I am event processor"




-- bouncecallback :: IORef (Iteratee Int IO ()) -> Int -> IO ()
bouncecallback :: IORef (Await Int (Iteratee Int IO ())) -> Int -> IO () 
bouncecallback tref input = do 
  Await cont <- readIORef tref 
  nr <- resume (cont input) 

  --  putStrLn "h" 
  -- r <- resume tr
  case nr of  
    Left  naw -> writeIORef tref naw 
    Right val -> do putStrLn $ show val 
                    writeIORef tref (Await (\_ -> return ()))
  putStrLn "one step"
  return ()  



{-
bouncecallback :: IORef (Trampoline IO ()) ->  IO () 
bouncecallback tref = do 
  tr <- readIORef tref 
  r <- resume tr
  case r of  
    Left tr' -> writeIORef tref (runIdentity tr') 
    Right val -> putStrLn $ show val 
  putStrLn "one step"
  return ()  
-}

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
  r <- bounce (either (gen
  case r of  
    Left (n,gen') -> writeIORef gref gen' 
    Right val -> putStrLn $ show val 
  putStrLn "one step"
  return ()  

-}

rollDice :: IO Int 
rollDice = getStdRandom (randomR (1,6))

eventprocessor :: (Int -> IO ()) -> IO () 
eventprocessor callback = do 
  -- putStrLn "I am event processor"
  r <- rollDice
  putStrLn ("rollDice r = " ++ show r)
  callback r
  threadDelay 5000000
  eventprocessor callback 

