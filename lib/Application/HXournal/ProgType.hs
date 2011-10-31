{-# LANGUAGE DeriveDataTypeable #-}

module Application.HXournal.ProgType where 

import System.Console.CmdArgs

data Hxournal = Test 
--               | CoroutineTest
              deriving (Show,Data,Typeable)

test :: Hxournal
test = Test 

-- coroutineTest :: Hxournal
--  coroutineTest = CoroutineTest

mode :: Hxournal
mode = modes [test] -- , coroutineTest]

