module Main where

import Control.Monad
import Control.Monad.State
import Data.Attoparsec
import qualified Data.ByteString as B
import Data.Hoodle.Simple
import Graphics.Hoodle.Render
import Graphics.Rendering.Cairo
import System.Environment
import Text.Hoodle.Parse.Attoparsec

-- |
main :: IO ()
main = do
  args <- getArgs
  when (length args /= 3) $ error "print1page mod infile outfile (mod = svg/pdf/png"
  let mod = head args
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
            runStateT (renderPageStateT fstpage) ctxt
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
