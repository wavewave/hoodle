{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (filterM)
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import qualified Graphics.UI.Gtk as Gtk (initGUI)
--
import Hoodle.Publish.PDF
import System.Console.CmdArgs
import System.Directory (removeFile)
import System.Directory.Tree (AnchoredDirTree (..), build, flattenDir)
import System.FilePath (makeRelative, replaceExtension, (</>))

--

data HoodlePublish = Publish
  { urlbase :: String,
    rootpath :: FilePath,
    buildpath :: FilePath,
    specialurlbase :: String
  }
  deriving (Show, Data, Typeable)

publish :: HoodlePublish
publish =
  Publish
    { urlbase = def &= typ "URLBASE" &= argPos 0,
      rootpath = def &= typ "ORIGNALFILEDIR" &= argPos 1,
      buildpath = def &= typ "TARGETFILEDIR" &= argPos 2,
      specialurlbase = def &= typ "SPECIALURLBASE"
    }

mode :: HoodlePublish
mode = modes [publish]

-- |
main :: IO ()
main = do
  Gtk.initGUI
  params <- cmdArgs mode
  (_r :/ r') <- build (rootpath params)
  let files = mapMaybe takeFile . flattenDir $ r'
      hdlfiles = filter isHdl files
      pairs =
        map
          ( (,) <$> id
              <*> (buildpath params </>) . flip replaceExtension "pdf" . makeRelative (rootpath params)
          )
          hdlfiles
      swappedpairs = map (\(x, y) -> (y, x)) pairs
  (_b :/ b') <- build (buildpath params)
  let files2 = mapMaybe takeFile . flattenDir $ b'
      pdffiles = filter isPdf files2
      willbeerased = filter (\x -> isNothing (lookup x swappedpairs)) pdffiles
  mapM_ removeFile willbeerased
  updatedpairs <- filterM isUpdated pairs
  mapM_ (createPdf (urlbase params, specialurlbase params) (rootpath params)) updatedpairs
