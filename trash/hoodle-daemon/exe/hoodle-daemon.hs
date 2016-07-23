{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where 

import           Control.Applicative ((<$>))
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.Chan (newChan, readChan, Chan)
import           Control.Monad (forever, when)
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F (forM_)
import           Data.Word (Word32)
import           Data.Maybe (mapMaybe)
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           DBus
import           DBus.Client
import           Filesystem.Path ((</>))
import           Network.Simple.TCP (HostPreference(..))
import           System.Directory
import qualified System.FilePath as F ((</>))
import           System.Process
--
import           Hub
import           ImageFileNotify
import           Network
import qualified Window as Window


isInitialized :: IO ()
isInitialized = do putStrLn "attempt to initialize another instance" 
                   return ()

checkOtherInst :: Client -> (Client -> IO ()) -> IO ()
checkOtherInst client act = do 
  ereply <- call client
              (methodCall "/hoodleDaemon" "org.ianwookim.hoodle" "isInitialized")
              { methodCallDestination = Just "org.ianwookim.hoodle-daemon"
              , methodCallBody = [] }
  case ereply of 
    Left _  -> do putStrLn "no pre-existing instances" 
                  putStrLn "starting new instance" 
                  act client 
    Right _ -> putStrLn "existing instance"
    
sendNetworkEditHoodle :: Client -> Chan T.Text -> IO ()
sendNetworkEditHoodle cli chan = do 
  forever $ do 
    txt <- readChan chan
    emit cli (signal "/networkedit" "org.ianwookim.hoodle" "latex") { signalBody = [toVariant txt] }
  
main :: IO ()
main = do 
  putStrLn "start hoodle-daemon"
  clientUsr <- connectSession 
  clientSys <- connectSystem
  homedir <- getHomeDirectory
  --  
  checkOtherInst clientUsr $ \client -> do 
    requestName client "org.ianwookim.hoodle-daemon" [] 
    export client "/hoodleDaemon"
      [ autoMethod "org.ianwookim.hoodle" "isInitialized" isInitialized ] 
    --
    chan <- newChan
    forkIO $ 
      startImageFileNotify chan (fromString homedir </> "Dropbox" </> "Apps" </> "Cambox") 
    forkIO $
      workChan clientUsr chan 
    -- 
    ip <- ipfind   
    let texfile = (homedir F.</> ".hoodle.d" F.</> "default.tex")
    b <- doesFileExist texfile
    ntxt <- if b 
            then TE.decodeUtf8 <$> B.readFile texfile
            else return defaultText
    --       
    chan_net <- newChan
    forkIO $ forever $ serverLaTeX (Host ip) ntxt chan_net
    forkIO $ forever $ sendNetworkEditHoodle clientUsr chan_net
    -- 
    chan_title <- newChan 
    forkIO $ Window.server client 
    forkIO $ Window.serverLink client
    -- 
    forkIO $ Hub.receiveHub

    putStrLn "hello"
    getLine >> return ()
     
