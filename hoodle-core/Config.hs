{-# LANGUAGE ScopedTypeVariables #-}

module Config where

import Data.List 
import Data.Maybe

import Control.Applicative
import Control.Monad


import Distribution.Simple
import Distribution.Simple.BuildPaths

import Distribution.Simple.Setup
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

import Distribution.System

import System.Exit
import System.Process

import System.FilePath
import System.Directory

import System.Info

myConfigHook :: UserHooks
myConfigHook = simpleUserHooks { 
  confHook = hookfunction
} 

hookfunction :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags 
                -> IO LocalBuildInfo
hookfunction x cflags = do 
  binfo <- confHook simpleUserHooks x cflags
  let pkg_descr = localPkgDescr binfo
  r_pbi <- config binfo
  let newbinfo = 
        case r_pbi of 
          Just pbi -> binfo { localPkgDescr = 
                                updatePackageDescription pbi pkg_descr }
          Nothing -> do 
            let r_lib = library pkg_descr 
            case r_lib of
              Just lib ->  
                let binfo2 = libBuildInfo lib
                    newlib = lib { libBuildInfo = binfo2 { cSources = [] }}  
                in  binfo {localPkgDescr = pkg_descr {library = Just newlib}}
              Nothing -> error "some library setting is wrong."  
  return newbinfo 


config :: LocalBuildInfo -> IO (Maybe HookedBuildInfo)
config bInfo = do 
  let cflags = (configConfigurationsFlags . configFlags) bInfo
      -- b = maybe False id (lookup (FlagName "gtk3") cflags)
  incdirs <- -- if b 
             --   then
             do
    (excode,out,err) <- readProcessWithExitCode "pkg-config" ["--cflags", "gtk+-3.0"] ""
    case excode of 
     ExitSuccess -> return . extractIncDir $ out
     _ -> error $ ("failure when running pkg-config --cflags gtk+-3.0 :\n" ++ err) 

             -- else do 
             --     (excode,out,err) <- readProcessWithExitCode "pkg-config" ["--cflags", "gtk+-2.0"] "" 
              --    case excode of 
               --     ExitSuccess -> return . extractIncDir $ out
               --     _ -> error $ ("failure when running pkg-config --cflags gtk+-2.0 :\n" ++ err) 

 
  let Just lib = library . localPkgDescr $ bInfo
      buildinfo = libBuildInfo lib
  let hbi = emptyBuildInfo { extraLibs = extraLibs buildinfo 
                           , extraLibDirs = extraLibDirs buildinfo
                           , includeDirs = incdirs ++ includeDirs buildinfo 
                           }
  let (r :: Maybe HookedBuildInfo) = Just (Just hbi, []) 
  -- putStrLn $ show hbi
  return r 

-- data CFlagsOptionSet = CFlagsOptionSet { 
--    includedirs :: [String] 
--    others :: [String]
--  }

extractIncDir :: String -> [String]
extractIncDir = (mapMaybe parseCFlags) . words 

parseCFlags :: String -> Maybe String 
parseCFlags [] = Nothing 
parseCFlags str@(x:xs) = 
  case x of 
    '-' -> if null xs 
             then Nothing 
             else let (y:ys) = xs 
                  in case y of
                    'I' -> Just ys 
                    _ -> Nothing 
    _ -> Nothing 
