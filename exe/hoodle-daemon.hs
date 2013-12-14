{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import DBus
import DBus.Client

main :: IO ()
main = do
  putStrLn "hoodle daemon version 0.0.999"
  
  client <- connectSession 
  
  reply <- call_ client (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
    { methodCallDestination = Just "org.freedesktop.DBus"
    }
           
  let Just names = fromVariant (methodReturnBody reply !! 0)
  mapM_ putStrLn (sort names)
  

