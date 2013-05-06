{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Config 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Config where

import Control.Monad
import Data.Configurator as C
import Data.Configurator.Types 
import System.Environment 
import System.Directory
import System.FilePath
import Control.Concurrent 

emptyConfigString :: String 
emptyConfigString = "\n#config file for hoodle \n "  

loadConfigFile :: IO Config
loadConfigFile = do 
  homepath <- getEnv "HOME" 
  let dothoodle = homepath </> ".hoodle"
  b <- doesFileExist dothoodle
  when (not b) $ do 
    writeFile dothoodle emptyConfigString 
    threadDelay 1000000
  config <- load [Required "$(HOME)/.hoodle"]
  return config
  
getMaxUndo :: Config -> IO (Maybe Int)
getMaxUndo c = C.lookup c "maxundo"

getPenDevConfig :: Config -> IO (Maybe String, Maybe String,Maybe String,Maybe String) 
getPenDevConfig c = do 
  mcore <- C.lookup c "core"
  mstylus <- C.lookup c "stylus" 
  meraser <- C.lookup c "eraser"
  mtouch <- C.lookup c "touch"
  return (mcore,mstylus,meraser,mtouch)
  
getXInputConfig :: Config -> IO Bool 
getXInputConfig c = do 
  (mxinput :: Maybe String) <- C.lookup c "xinput"
  case mxinput of
    Nothing -> return False
    Just str -> case str of 
                  "true" -> return True
                  "false" -> return False 
                  _ -> error "cannot understand xinput in configfile"

getWidgetConfig :: Config -> IO (Bool, Bool) 
getWidgetConfig c = do 
    (mpanzoom :: Maybe String) <- C.lookup c "PanZoomWidget"
    (mlayer :: Maybe String) <- C.lookup c "LayerWidget"

    let panzoom = maybe True (parse "PanZoomWidget") mpanzoom 
        layer = maybe True (parse "LayerWidget") mlayer
        
    print ("WidgetConfig =" ++ show (panzoom,layer) )
    return (panzoom,layer)
  where parse msg str = case str of
                          "true" -> True
                          "false" -> False 
                          _ -> error ("cannot understand " ++ msg ++ " in configfile")

         

{-
getNetworkInfo :: Config -> IO (Maybe HoodleClipClientConfiguration)
getNetworkInfo c = do 
  (mserver :: Maybe String) <- C.lookup c "network.server"
  (mclient :: Maybe String) <- C.lookup c "network.client"
  return (HoodleClipClientConfiguration <$> mserver <*> mclient)
-}
