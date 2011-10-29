{-# LANGUAGE DeriveDataTypeable #-}

module Application.HXournal.Type where 

import System.Console.CmdArgs

data Hxournal = Test 
              deriving (Show,Data,Typeable)

test :: Hxournal
test = Test 

mode = modes [test]

