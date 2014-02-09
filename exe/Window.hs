{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Window
-- Copyright   : (c) 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Window where 

import           Control.Concurrent
import           Control.Monad (when)
import qualified Data.Map as M 
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           DBus
import           DBus.Client
import           System.Exit
import           System.Process

data WindowInfo = WInfo { windowId :: String
                        , desktopId :: String
                        , processId :: String 
                        , hostName :: String
                        , windowTitle :: String } 
		deriving (Show,Eq,Ord)

{-server client = do 
  requestName client "org.ianwookim" []
-}

server :: Client -> IO ()
server client = do 
  listen client matchAny { matchInterface = Just "org.ianwookim.hoodle" 
                                  , matchMember = Just "findWindow" 
                                  } 
         findWindow 

findWindow :: Signal -> IO ()
findWindow sig = do
    let txts = mapMaybe fromVariant (signalBody sig) :: [String] -- :: [T.Text]  
    when ((not.null) txts) $ do
      print (head txts)
  

getWindow :: String -> IO (Maybe String) 
getWindow title = do 
  putStrLn "Window server process"
  (excode,sout,serr) <- readProcessWithExitCode "wmctrl" [ "-l", "-p" ] ""
  case excode of 
    ExitSuccess -> do 
      let xs  = lines sout
          mkWInfo x =
             let rest0 = dropWhile (== ' ') x
                 (wid,rest1) = break (== ' ') rest0  
                 (did,rest2) = (break (== ' ') . dropWhile (== ' ')) rest1 
                 (pid,rest3) = (break (== ' ') . dropWhile (== ' ')) rest2 
                 (hname,rest4) = (break (== ' ') . dropWhile (== ' ')) rest3 
                 title = dropWhile (== ' ') rest4 
             in WInfo wid did pid hname title
          insertToMap x = M.insert (windowTitle x) x 
          wmap = foldr insertToMap M.empty (map mkWInfo xs)
      case M.lookup title wmap of 
        Nothing -> return Nothing
        Just w  -> return (Just (windowId w))

    ExitFailure _ -> return Nothing





