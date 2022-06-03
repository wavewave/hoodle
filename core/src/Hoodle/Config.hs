{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hoodle.Config where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import qualified Data.Configurator as C
import Data.Configurator.Types (Config, Worth (Required))
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import System.FilePath ((</>))

emptyConfigString :: String
emptyConfigString = "\n#config file for hoodle \n "

loadConfigFile :: IO Config
loadConfigFile = do
  homepath <- getEnv "HOME"
  let dothoodle = homepath </> ".hoodle"
  b <- doesFileExist dothoodle
  unless b $ do
    writeFile dothoodle emptyConfigString
    threadDelay 1000000
  C.load [Required "$(HOME)/.hoodle"]

getMaxUndo :: Config -> IO (Maybe Int)
getMaxUndo c = C.lookup c "maxundo"

getPenDevConfig :: Config -> IO (Maybe String, Maybe String, Maybe String, Maybe String)
getPenDevConfig c = do
  mcore <- C.lookup c "core"
  mstylus <- C.lookup c "stylus"
  meraser <- C.lookup c "eraser"
  mtouch <- C.lookup c "touch"
  return (mcore, mstylus, meraser, mtouch)

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
  let panzoom = maybe True (parseBool "PanZoomWidget") mpanzoom
      layer = maybe True (parseBool "LayerWidget") mlayer
  print ("WidgetConfig =" ++ show (panzoom, layer))
  return (panzoom, layer)

parseBool :: String -> String -> Bool
parseBool msg str = case str of
  "true" -> True
  "false" -> False
  _ -> error ("cannot understand " ++ msg ++ " in configfile")

getPenConfig :: Config -> IO Bool
getPenConfig c = do
  (mvcursor :: Maybe String) <- C.lookup c "variablecursor"
  let vcursor = maybe False (parseBool "variablecursor") mvcursor
  return vcursor

{-
getNetworkInfo :: Config -> IO (Maybe HoodleClipClientConfiguration)
getNetworkInfo c = do
  (mserver :: Maybe String) <- C.lookup c "network.server"
  (mclient :: Maybe String) <- C.lookup c "network.client"
  return (HoodleClipClientConfiguration <$> mserver <*> mclient)
-}
