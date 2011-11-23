{-# LANGUAGE DeriveDataTypeable #-}

module Application.HXournal.ProgType where 

import System.Console.CmdArgs

data Hxournal = Test    { xojfile :: FilePath
                        }  
{-              | MakeSVG { xojfile :: FilePath
                        } -}
              | TestBuilder { xojfile :: FilePath }
              deriving (Show,Data,Typeable)

test :: Hxournal
test = Test { xojfile = "test.xoj" &= typ "FILENAME" &= argPos 0  
            } &= auto 

{-
makeSVG :: Hxournal
makeSVG = MakeSVG { xojfile = "test.xoj" &= typ "FILENAME" &= argPos 0 
                  }
-}

testbuilder :: Hxournal 
testbuilder = TestBuilder { xojfile = "test.xoj" &= typ "FILENAME" &= argPos 0 
                          } 

mode :: Hxournal
mode = modes [test, testbuilder]


