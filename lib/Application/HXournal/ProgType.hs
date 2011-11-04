{-# LANGUAGE DeriveDataTypeable #-}

module Application.HXournal.ProgType where 

import System.Console.CmdArgs

data Hxournal = Test    { xojfile :: FilePath
                        }  
              | MakeSVG { xojfile :: FilePath
                        }
              deriving (Show,Data,Typeable)

test :: Hxournal
test = Test { xojfile = "test.xoj" &= typ "FILENAME" &= argPos 0  
            }

makeSVG :: Hxournal
makeSVG = MakeSVG { xojfile = "test.xoj" &= typ "FILENAME" &= argPos 0 
                  }

mode :: Hxournal
mode = modes [test, makeSVG]


