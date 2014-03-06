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
import qualified Data.Foldable as F (mapM_,forM_)
import           Data.List.Split (splitOn)
import qualified Data.Map as M 
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import           DBus
import           DBus.Client
import           System.Exit
import           System.FilePath 
import           System.Process

data WindowInfo = WInfo { windowId :: String
                        , desktopId :: String
                        , processId :: String 
                        , hostName :: String
                        , windowTitle :: String } 
		deriving (Show,Eq,Ord)

server :: Client -> IO ()
server client = do 
    listen client matchAny { matchInterface = Just "org.ianwookim.hoodle" 
                           , matchMember = Just "findWindow" 
                           } 
           findWindow 

findWindow :: Signal -> IO ()
findWindow sig = do
    let txts = mapMaybe fromVariant (signalBody sig) :: [String] 
    when ((not.null) txts) $ do
      let fpath = head txts
          (fdir,fname) = splitFileName fpath 
      print fname 
      mwid <- getWindow fname
      print mwid
      case mwid of 
        Nothing -> createProcess (proc "hoodle" [fpath]) 
                   >> return ()    
        Just wid -> do 
          (excode,sout,serr) <- readProcessWithExitCode "wmctrl" [ "-i", "-a", wid ] ""
          print excode 
          return ()

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
      F.mapM_ print wmap
      case M.lookup title wmap of 
        Nothing -> return Nothing
        Just w  -> return (Just (windowId w))

    ExitFailure _ -> return Nothing


serverLink :: Client -> IO ()
serverLink client = do
    putStrLn "start serverLink"
    listen 
      client 
      matchAny { matchInterface = Just "org.ianwookim.hoodle"
               , matchMember = Just "callLink" }
      (callLink client)
      

callLink :: Client -> Signal -> IO ()
callLink cli sig = do
    let txts = mapMaybe fromVariant (signalBody sig) :: [String]
    case txts of 
      txt : _ -> do 
        let  r = splitOn "," txt
        case r of 
          docid:anchorid:_ -> do 
            emit cli (signal "/" "org.ianwookim.hoodle" "link")
                       { signalBody = [ toVariant txt ] }  
            putStrLn ( docid ++ ":" ++ anchorid)
          _ -> return ()
      _ -> return ()