{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Application.HXournal.Config where

import Control.Monad
import Data.Configurator as C
import Data.Configurator.Types 
import System.Environment 
import System.Directory
import System.FilePath
import Control.Concurrent 

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
  
getPenDevConfig :: Config -> IO (Maybe String, Maybe String,Maybe String) 
getPenDevConfig c = do 
  mcore <- C.lookup c "core"
  mstylus <- C.lookup c "stylus" 
  meraser <- C.lookup c "eraser"
  return (mcore,mstylus,meraser)
  

