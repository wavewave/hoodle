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
import qualified Data.Map as M 
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

server = getWindow 

getWindow :: Chan String -> IO () 
getWindow chan = do 
  title <- readChan chan  
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
        Nothing -> return ()
        Just w  -> print (windowId w)

    ExitFailure _ -> return ()





