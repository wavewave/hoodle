{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : Hoodle.ProgType
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
module Hoodle.ProgType where

import Data.Version (showVersion)
--
import Paths_hoodle (version)
import System.Console.CmdArgs
  ( Data,
    Typeable,
    args,
    auto,
    def,
    explicit,
    help,
    helpArg,
    modes,
    name,
    program,
    summary,
    typ,
    versionArg,
    (&=),
  )

-- TODO: rename xojfile and Test. use optparse-applicative.
data Hoodle = Test
  { xojfile :: Maybe FilePath
  }
  deriving (Show, Data, Typeable)

programName :: String
programName = "Hoodle"

programVersion :: String
programVersion = showVersion version

programInfo :: String
programInfo = programName ++ " version " ++ programVersion

programAbout :: String
programAbout = "A pen notetaking program written in haskell"

programCopyright :: String
programCopyright = "(C) Ian-Woo Kim 2011-2013"

test :: Hoodle
test =
  Test
    { xojfile = def &= typ "FILENAME" &= args
    }
    &= auto

mode :: Hoodle
mode =
  modes [test]
    &= versionArg [explicit, name "version", name "V", summary programInfo]
    &= summary (programInfo ++ ", " ++ programCopyright)
    &= help programAbout
    &= helpArg [explicit, name "help", name "h"]
    &= program programName
