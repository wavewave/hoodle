import Control.Monad
import Data.Attoparsec
import qualified Data.ByteString as B
import Graphics.Rendering.Cairo
import System.Environment
-- from hoodle-platform 
import Data.Hoodle.Simple 
import Text.Hoodle.Parse.Attoparsec 
-- from this package
import Graphics.Hoodle.Render.Simple 


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
    Just h -> do 
      let fstpage = head (hoodle_pages h) 
          Dim width height = page_dim fstpage 
      let action 
            | mod == "svg" = withSVGSurface outfile width height 
                               (\s -> renderWith s (cairoDrawPage fstpage))
            | mod == "pdf" = withPDFSurface outfile width height
                               (\s -> renderWith s (cairoDrawPage fstpage))  
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
