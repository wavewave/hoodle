{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Werror -Wall #-}

-- This checks unclean imports
-- applies to GHC 8.8.

module Main (main) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import DynFlags (DynFlags)
import qualified DynFlags
import qualified Extensions.Module as EM
import qualified Extensions.Types as E
import FastString (mkFastString)
import qualified GHC
import qualified GHC.Paths as GHC.Paths
import HsExtension (GhcPs)
import HsImpExp (ImportDecl (..))
import HsSyn (HsModule (..))
import Lexer (P (unP), ParseResult (..), mkPState)
import qualified Options.Applicative as OA
import Outputable (ppr, showSDoc)
import qualified Parser
import SrcLoc (mkRealSrcLoc, unLoc)
import StringBuffer (stringToStringBuffer)
import System.Exit (exitFailure)

newtype Options = Options {sourceFileName :: FilePath}

pOptions :: OA.Parser Options
pOptions =
  Options
    <$> OA.strOption (OA.long "input" <> OA.short 'i' <> OA.help "source file")

runParser :: DynFlags -> String -> FilePath -> P a -> ParseResult a
runParser flags str filename parser = unP parser parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState flags buffer location

isImplicitImport :: ImportDecl GhcPs -> Bool
isImplicitImport (ImportDecl {ideclQualified, ideclHiding})
  | ideclQualified = False
  | Just (False, _) <- ideclHiding = False
  | otherwise = True
isImplicitImport _ = False

main :: IO ()
main = do
  opts <- OA.execParser (OA.info pOptions mempty)
  let srcfile = sourceFileName opts
  putStrLn $ "checking " <> srcfile
  contents <- readFile srcfile

  eParsedExtensions <- EM.parseFile srcfile
  case eParsedExtensions of
    Left err -> print err
    Right pexts -> do
      let exts = E.parsedExtensionsAll pexts
      GHC.runGhc (Just GHC.Paths.libdir) $ do
        _env <- GHC.getSession
        dflags <- GHC.getSessionDynFlags
        let dflagsWithExts =
              let addExts =
                    foldr (.) id $
                      fmap
                        (\case E.On e -> flip DynFlags.xopt_set e; E.Off e -> flip DynFlags.xopt_unset e)
                        exts
               in addExts dflags
        let presult = runParser dflagsWithExts contents srcfile Parser.parseModule
        case presult of
          POk _ r -> liftIO $ do
            let modu :: HsModule GhcPs
                modu = unLoc r
                implicitImports = filter (isImplicitImport . unLoc) $ hsmodImports modu
            unless (null implicitImports) $ do
              putStrLn "implicit imports"
              putStrLn "----------------"
              for_ implicitImports $ \imp ->
                putStrLn $ showSDoc dflags (ppr imp)
              putStrLn "----------------"
              exitFailure
          _ -> do
            liftIO $ putStrLn "Parse Failed"
            liftIO exitFailure
