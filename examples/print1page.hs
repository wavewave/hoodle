import Control.Monad
import Control.Monad.State 
import Data.Attoparsec
import qualified Data.ByteString as B
import Graphics.Rendering.Cairo
import System.Environment
-- from hoodle-platform 
import Data.Hoodle.Simple 
import Text.Hoodle.Parse.Attoparsec 
-- from this package
import Graphics.Hoodle.Render.SimpleNew 


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
      let fstpage = head (hoodle_pages hoo) 
          Dim w h = page_dim fstpage 
          cairowork s = renderWith s (evalStateT (cairoDrawPage fstpage) ())
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
