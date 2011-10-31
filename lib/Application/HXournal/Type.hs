{-# LANGUAGE DeriveDataTypeable #-}

module Application.HXournal.Type where 

import System.Console.CmdArgs

data Hxournal = Test 
              | CoroutineTest
              deriving (Show,Data,Typeable)

test :: Hxournal
test = Test 

coroutineTest :: Hxournal
coroutineTest = CoroutineTest

mode = modes [test, coroutineTest]

