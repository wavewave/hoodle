{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plugin.CheckImports (plugin) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import CoreMonad
  ( getDynFlags,
    getHscEnv,
    getModule,
    getOrigNameCache,
  )
import Data.Char (isAlpha)
import Data.Foldable (for_)
import Data.IORef (readIORef)
import Data.List (concat, foldl', intercalate, sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as S
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
import IOEnv (failWithM)
import Module (ModuleName, lookupModuleEnv)
import Name (Name, getName, localiseName)
import Outputable (Outputable (ppr), showSDoc)
import RdrName
  ( GlobalRdrElt (..),
    GlobalRdrEnv,
    ImpDeclSpec (..),
    ImportSpec (..),
    greLabel,
    pprGlobalRdrEnv,
    pprNameProvenance,
  )
import TcRnTypes (IfM, TcGblEnv (..), TcM)

plugin :: Plugin
plugin =
  defaultPlugin
    { typeCheckResultAction = typecheckPlugin
    }

printPpr :: (Outputable a, MonadIO m) => DynFlags -> a -> m ()
printPpr dflags = liftIO . putStrLn . showSDoc dflags . ppr

formatName :: DynFlags -> Name -> String
formatName dflags name =
  let str = showSDoc dflags . ppr . localiseName $ name
   in case str of
        (x : xs) ->
          if isAlpha x
            then str
            else "(" <> str <> ")"
        _ -> str

formatImportedNames :: [String] -> String
formatImportedNames names =
  case fmap (<> ",\n") $ sort names of
    line0 : lines ->
      let line0' = "  ( " <> line0
          lines' = fmap ("    " <>) lines
          footer = "  )"
       in concat ([line0'] <> lines' <> [footer])
    _ -> "  ()"

mkModuleNameMap :: GlobalRdrElt -> [(ModuleName, Name)]
mkModuleNameMap gre = do
  spec <- gre_imp gre
  let modName = is_mod $ is_decl spec
  pure (modName, gre_name gre)

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
  let moduleImportMap :: Map ModuleName (Set Name)
      moduleImportMap =
        foldl' (\(!m) (modu, name) -> M.insertWith S.union modu (S.singleton name) m) M.empty $
          concatMap mkModuleNameMap usedGREs
  for_ (M.toList moduleImportMap) $ \(modu, names) -> liftIO $ do
    putStrLn "---------"
    printPpr dflags modu
    let imported = fmap (formatName dflags) $ S.toList names
    putStrLn $ formatImportedNames imported
  printPpr dflags modsummary
  failWithM "force fail"
  pure tc
