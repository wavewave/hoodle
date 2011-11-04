{-# LANGUAGE DeriveDataTypeable #-}

module Application.HXournal.ProgType where 

import System.Console.CmdArgs

data Hxournal = Test { xojfile :: FilePath
               
                     } 
--               | CoroutineTest
              deriving (Show,Data,Typeable)

test :: Hxournal
test = Test { xojfile = "test.xoj" &= typ "FILENAME" &= argPos 0  
            }

-- coroutineTest :: Hxournal
--  coroutineTest = CoroutineTest

mode :: Hxournal
mode = modes [test] -- , coroutineTest]

