{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forever)
import Data.List (sort)
import DBus
import DBus.Client

signalCallback :: Signal -> IO ()
signalCallback signal_ = putStrLn $ "Received signal: " ++ show sig
  where 
    sig :: String 
    Just sig = fromVariant $ head $ signalBody signal_ 
    

main :: IO ()
main = do
  putStrLn "hoodle daemon version 0.0.999"
  
  client <- connectSession 
  listen client matchAny signalCallback 
  
  forever $ getLine 
 
  {-
  
  reply <- call_ client (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
    { methodCallDestination = Just "org.freedesktop.DBus"
    }
           
  let Just names = fromVariant (methodReturnBody reply !! 0)
  mapM_ putStrLn (sort names)
  
-}

