{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.List (sort)
import DBus
import DBus.Client

isInitialized :: IO ()
isInitialized = do putStrLn "attempt to initialize another instance" 
                   return ()

checkOtherInst :: Client -> (Client -> IO ()) -> IO ()
checkOtherInst client act = do 
  ereply <- call client
              (methodCall "/hoodleDaemon" "org.ianwookim" "isInitialized")
              { methodCallDestination = Just "org.ianwookim"
              , methodCallBody = [] }
  case ereply of 
    Left err -> do putStrLn "no pre-existing instances" 
                   putStrLn "starting new instance" 
                   act client 
    Right _ -> putStrLn "existing instance"
    
onResume :: Signal -> IO ()
onResume signal = do print signal
                     putStrLn "I got back from sleep mode"

  
  
  
main :: IO ()
main = do 
  clientUsr <- connectSession 
  clientSys <- connectSystem
  
  forkIO $ do 
    listen clientSys matchAny { matchPath = Just "/org/freedesktop/UPower" 
                              , matchMember = Just "NotifyResume" 
                              }
           onResume 
    forever $ getLine
  
  
  checkOtherInst clientUsr $ \client -> do 
    requestName client "org.ianwookim" [] 
    export client "/hoodleDaemon"
      [ autoMethod "org.ianwookim" "isInitialized" isInitialized  
      ] 
    forever $ getLine

-- square :: Double -> IO Double
-- square x = return $ x**2 

{- 
main :: IO ()
main = do 
  client <- connectSession
  requestName client "org.jonte.math" [] 
  
  export client "/math_object"
    [ autoMethod "org.jonte.math" "square" square
    ] 
    
  forever $ getLine
-}
  
  
{-
signalCallback :: Signal -> IO ()
signalCallback signal_ = putStrLn $ "Received signal: " ++ show sig
  where 
    sig :: String 
    Just sig = fromVariant $ head $ signalBody signal_ 
    

main :: IO ()
main = do
  putStrLn "hoodle daemon version 0.0.999"
  
  client <- connectSession 
  listen client (matchAny { matchSender = Just "org.ianwookim.test"}) signalCallback 
  
  forever $ getLine 
-}


  {-
  
  reply <- call_ client (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
    { methodCallDestination = Just "org.freedesktop.DBus"
    }
           
  let Just names = fromVariant (methodReturnBody reply !! 0)
  mapM_ putStrLn (sort names)
  
-}

