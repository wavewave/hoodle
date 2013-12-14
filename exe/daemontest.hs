{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import DBus
import DBus.Client 


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
  
