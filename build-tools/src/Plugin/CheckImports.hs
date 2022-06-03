{-# LANGUAGE ScopedTypeVariables #-}

module Plugin.CheckImports (plugin) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import CoreMonad
  ( getDynFlags,
    getHscEnv,
    getModule,
    getOrigNameCache,
  )
import Data.Foldable (for_)
import Data.IORef (readIORef)
import DynFlags (DynFlags)
import GHC (getModuleInfo)
import GhcPlugins
  ( CommandLineOption,
    CoreM,
    CoreToDo,
    HsParsedModule,
    Hsc,
    ModSummary,
    Plugin (..),
    defaultPlugin,
    putMsgS,
  )
import HsDecls (HsGroup (..))
import HsExtension (GhcRn)
import HscTypes (HsParsedModule (..), ModIface (..))
import Module (lookupModuleEnv)
import Outputable (Outputable (ppr), showSDoc)
import RdrName
  ( GlobalRdrElt (..),
    GlobalRdrEnv,
    greLabel,
    pprGlobalRdrEnv,
    pprNameProvenance,
  )
import TcRnTypes (IfM, TcGblEnv (..), TcM)

plugin :: Plugin
plugin =
  defaultPlugin
    { parsedResultAction = parsedPlugin,
      renamedResultAction = renamedAction,
      typeCheckResultAction = typecheckPlugin,
      interfaceLoadAction = interfaceLoadPlugin
      -- installCoreToDos = install
    }

printPpr :: (Outputable a, MonadIO m) => DynFlags -> a -> m ()
printPpr dflags = liftIO . putStrLn . showSDoc dflags . ppr

parsedPlugin ::
  [CommandLineOption] ->
  ModSummary ->
  HsParsedModule ->
  Hsc HsParsedModule
parsedPlugin _ _ mod = do
  liftIO $ putStrLn "my TEst"
  dflags <- getDynFlags
  printPpr dflags (hpm_module mod)
  pure mod

renamedAction ::
  [CommandLineOption] ->
  TcGblEnv ->
  HsGroup GhcRn ->
  TcM (TcGblEnv, HsGroup GhcRn)
renamedAction _ env grp = do
  liftIO $ putStrLn "renamed action invoked"
  dflags <- getDynFlags
  printPpr dflags grp
  printPpr dflags (hs_valds grp)
  pure (env, grp)

interfaceLoadPlugin ::
  [CommandLineOption] ->
  ModIface ->
  IfM lcl ModIface
interfaceLoadPlugin _ iface = do
  dflags <- getDynFlags
  liftIO $ putStrLn "interface loaded"
  printPpr dflags (mi_module iface)
  return iface

typecheckPlugin ::
  [CommandLineOption] ->
  ModSummary ->
  TcGblEnv ->
  TcM TcGblEnv
typecheckPlugin _ modsummary tc = do
  liftIO $ putStrLn "typecheck plugin"
  dflags <- getDynFlags
  let globalRdrEnv :: GlobalRdrEnv
      globalRdrEnv = tcg_rdr_env tc

  usedGREs :: [GlobalRdrElt] <-
    liftIO $ readIORef (tcg_used_gres tc)
  for_ usedGREs $ \gre -> liftIO $ do
    printPpr dflags (gre_name gre)
    putStrLn $ showSDoc dflags $ pprNameProvenance gre
  printPpr dflags modsummary
  pure tc

{-  env <- getHscEnv
  dflags <- getDynFlags
  let printPpr :: (Outputable a) => a -> CoreM ()
      printPpr = putMsgS . showSDoc dflags . ppr
  modu <- getModule
  printPpr modu
  nameCache <- getOrigNameCache
  let moccEnv = lookupModuleEnv nameCache modu
  case moccEnv of
    Nothing -> putMsgS "Nothing"
    Just occEnv -> printPpr occEnv
  -- moduInfo <- getModuleInfo modu
  -- printPpr moduInfo
  pure todo
-}
