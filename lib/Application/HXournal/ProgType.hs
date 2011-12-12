{-# LANGUAGE DeriveDataTypeable #-}

module Application.HXournal.ProgType where 

import System.Console.CmdArgs

data Hxournal = Test { xojfile :: Maybe FilePath
                     }  
              deriving (Show,Data,Typeable)

test :: Hxournal
test = Test { xojfile = def &= typ "FILENAME" &= args -- &= argPos 0 &= opt "OPTIONAL" 
            } &= auto 


mode :: Hxournal
mode = modes [test] 


