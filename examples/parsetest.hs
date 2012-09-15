-- 
-- testing program for attoparsec and sax hoodle parser
-- 

import           Control.Monad
import           Data.Attoparsec 
import qualified Data.ByteString as B
import           System.Environment 
-- 
import Data.Hoodle.Simple
-- 
import Text.Hoodle.Parse.Attoparsec 
import Text.Hoodle.Parse.Conduit 

-- |  
main :: IO ()   
main = do 
  args <- getArgs 
  when (length args /= 2) $ error "parsertest mode filename (mode = atto/sax)"  
  if args !! 0 == "atto" 
    then attoparsec (args !! 1)
    else sax (args !! 1)
     
     
-- | using attoparsec without any built-in xml support 
attoparsec :: FilePath -> IO () 
attoparsec fp = do 
  bstr <- B.readFile fp
  let r = parse hoodle bstr
  
  case r of 
    Done _ h -> print (length (hoodle_pages h))
      
    _ -> print r 


-- | using sax (from xml-conduit)
sax :: FilePath -> IO () 
sax fp = do 
  r <- parseHoodleFile fp 
  case r of 
    Left err -> print err
    Right h -> print (length (hoodle_pages h))