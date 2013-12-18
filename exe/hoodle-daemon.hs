{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, when)
import Data.Foldable (forM_)
import Data.Word (Word32(..))
import Data.IORef
import Data.List (sort)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import DBus
import DBus.Client
import System.Directory
import System.Process


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
    
onResume :: IORef ProcessHandle -> Signal -> IO ()
onResume ref signal = do -- print signal
                         let xs :: [Dictionary] = (mapMaybe fromVariant . signalBody) signal
                         when ((not.null) xs) $ do 
                           let x = dictionaryItems (head xs)                               
                           case (lookup (toVariant ("State" :: T.Text) ) x) of 
                            Just v -> do let n = do v' <- fromVariant v :: Maybe Variant 
                                                    v'' <- fromVariant v' :: Maybe Word32
                                                    return v''
                                         print n
                                         forM_ n $ \n' -> 
                                           when (n' == 100) $ do 
                                             threadDelay 1000000
                                             ph <- readIORef ref
                                             terminateProcess ph
                                             putStrLn "network restart"
                                             ph' <- runsocket
                                             writeIORef ref ph'
                            _ -> return ()
                         
runsocket = do 
  hdir <- getHomeDirectory
  (_,_,_,ph) <- createProcess
    ((proc "/home/wavewave/repo/workspace/hoodle-publish/socket/pipesocketcli" []) 
     {env = Just [("DISPLAY",":0"), ("LIBOVERLAY_SCROLLBAR","0"), ("HOME",hdir)] })
  return ph
  
  
main :: IO ()
main = do 
  clientUsr <- connectSession 
  clientSys <- connectSystem
  
  
  
  checkOtherInst clientUsr $ \client -> do 
    requestName client "org.ianwookim" [] 
    export client "/hoodleDaemon"
      [ autoMethod "org.ianwookim" "isInitialized" isInitialized  
      ] 
    ph <- runsocket
    ref <- newIORef ph
    forkIO $ do 
      listen clientSys matchAny { matchPath = Just "/org/freedesktop/NetworkManager/Devices/0" 
                                , matchInterface = Just "org.freedesktop.NetworkManager.Device.Wireless"
                                , matchMember = Just "PropertiesChanged" 
                                }
             (onResume ref) 
    forever $ getLine
    

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

