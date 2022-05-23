{-# LANGUAGE PackageImports #-}

module Main where

import Control.Monad
import Control.Monad.State
import Data.Attoparsec
import qualified Data.ByteString as B
-- from hoodle-platform
import Data.Hoodle.Simple
-- from this package
-- import Graphics.Hoodle.Render.SimpleNew
import Graphics.Hoodle.Render
import Graphics.Rendering.Cairo
import System.Environment
import Text.Hoodle.Parse.Attoparsec

-- import Graphics.Hoodle.Render.Generic

-- |
main :: IO ()
main = do
  args <- getArgs
  when (length args /= 3) $ error "print1page mod infile outfile (mod = svg/pdf/png"
  let mod = args !! 0
      infile = args !! 1
      outfile = args !! 2
  mh <- attoparsec (args !! 1)
  case mh of
    Nothing -> print "not parsed"
    Just hoo -> do
      ctxt <- initRenderContext hoo
      let fstpage = head (hoodle_pages hoo)
          Dim w h = page_dim fstpage
          cairowork s = renderWith s $ do
            flip runStateT ctxt (renderPageStateT fstpage)
            return ()

      let action
            | mod == "svg" = withSVGSurface outfile w h cairowork
            | mod == "pdf" = withPDFSurface outfile w h cairowork
            | otherwise = return ()
      action

-- |
attoparsec :: FilePath -> IO (Maybe Hoodle)
attoparsec fp = do
  bstr <- B.readFile fp
  let r = parse hoodle bstr
  case r of
    Done _ h -> return (Just h)
    _ -> return Nothing
