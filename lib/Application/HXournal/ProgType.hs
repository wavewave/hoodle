{-# LANGUAGE DeriveDataTypeable #-}

module Application.HXournal.ProgType where 

import System.Console.CmdArgs

data Hxournal = Test { xojfile :: Maybe FilePath
                     }  
--              | TestBuilder { xojfile :: FilePath }
              deriving (Show,Data,Typeable)

test :: Hxournal
test = Test { xojfile = def &= typ "FILENAME" &= args -- &= argPos 0 &= opt "OPTIONAL" 
            } &= auto 

{-
testbuilder :: Hxournal 
testbuilder = TestBuilder { xojfile = "test.xoj" &= typ "FILENAME" &= argPos 0 
                          } 
-}

mode :: Hxournal
mode = modes [test] -- [test, testbuilder]


