module TextFileDB where

import Control.Applicative
import System.FilePath
import System.Directory

defaultDBFile :: IO FilePath 
defaultDBFile = (</> "Dropbox" </> "hoodleiddb.dat") <$> getHomeDirectory 

splitfunc :: String -> (String,(String,String))
splitfunc str = 
  let (str1,rest1) = break (==' ') str 
      (str2,rest2) = break (==' ') (tail rest1)
      str3 = read (tail rest2)
  in (str1,(str2,str3))

    
