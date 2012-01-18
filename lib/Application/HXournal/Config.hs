{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Config 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Application.HXournal.Config where

import Control.Monad
import Data.Configurator as C
import Data.Configurator.Types 
import System.Environment 
import System.Directory
import System.FilePath
import Control.Concurrent 
import Control.Applicative
-- import Application.HXournal.NetworkClipboard.Client.Config

emptyConfigString :: String 
emptyConfigString = "\n#config file for hxournal \n "  

loadConfigFile :: IO Config
loadConfigFile = do 
  homepath <- getEnv "HOME" 
  let dothxournal = homepath </> ".hxournal"
  b <- doesFileExist dothxournal
  when (not b) $ do 
    writeFile dothxournal emptyConfigString 
    threadDelay 1000000
  config <- load [Required "$(HOME)/.hxournal"]
  return config
  
getMaxUndo :: Config -> IO (Maybe Int)
getMaxUndo c = C.lookup c "maxundo"

getPenDevConfig :: Config -> IO (Maybe String, Maybe String,Maybe String) 
getPenDevConfig c = do 
  mcore <- C.lookup c "core"
  mstylus <- C.lookup c "stylus" 
  meraser <- C.lookup c "eraser"
  return (mcore,mstylus,meraser)
  
getXInputConfig :: Config -> IO Bool 
getXInputConfig c = do 
  (mxinput :: Maybe String) <- C.lookup c "xinput"
  case mxinput of
    Nothing -> return False
    Just str -> case str of 
                  "true" -> return True
                  "false" -> return False 
                  _ -> error "cannot understand xinput in configfile"

{-
getNetworkInfo :: Config -> IO (Maybe HXournalClipClientConfiguration)
getNetworkInfo c = do 
  (mserver :: Maybe String) <- C.lookup c "network.server"
  (mclient :: Maybe String) <- C.lookup c "network.client"
  return (HXournalClipClientConfiguration <$> mserver <*> mclient)
-}
