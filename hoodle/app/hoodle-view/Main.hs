{-# LANGUAGE GHC2021 #-}
{-# OPTIONS_GHC -w #-}

module Main (main) where

import Hoodle.Command (commandLineProcess)
import Hoodle.ProgType (Hoodle (Test))
import Hoodle.Script (defaultScriptConfig)
import Hoodle.StartUp (hoodleMain)
import Options.Applicative qualified as OA
-- import System.IO (FilePath)

newtype Options = Options
  { filePath :: FilePath
  }
  deriving Show

optsParser :: OA.ParserInfo Options
optsParser =
  OA.info
    (inner OA.<**> OA.helper)
    ( OA.fullDesc
        <> OA.progDesc "hoodle view"
    )
  where
    inner =
      Options
        <$> OA.strOption (OA.long "file" <> OA.metavar "FILE" <> OA.help "hoodle filename")

main :: IO ()
main = do
  opts <- OA.execParser optsParser

  putStrLn "hoodle-view"

  print opts

  commandLineProcess (Test (Just (filePath opts))) Nothing
