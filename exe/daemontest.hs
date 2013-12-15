{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import DBus
import DBus.Client 

square :: Client -> Double -> IO Double
square client n = do
  reply <- call_ client 
             (methodCall "/math_object" "org.jonte.math" "square")
             { methodCallDestination = Just "org.jonte.math"
             , methodCallBody = [toVariant n]}                        
  return (unwrapFirst reply)
  
unwrapFirst :: IsVariant a => MethodReturn -> a 
unwrapFirst ret = unwrapped 
  where Just unwrapped = fromVariant $ head $ methodReturnBody ret 
        
main :: IO ()
main = do 
  client <- connectSession
  
  squared <- square client 4
  
  putStrLn $ show squared
  


{-
emitter :: Client -> IO ()
emitter client = do
  emit client (signal "/signal_object" "org.ianwookim.signal" "signal")
    { signalBody = [toVariant ("Hello World" :: String)]}
  putStrLn "Emitted singal"
  threadDelay 1000000
  emitter client 
  
main :: IO () 
main = do 
  client <- connectSession
  requestName client "org.ianwookim.test" [] 
  forkIO $ emitter client
  
  forever $ getLine
-}
