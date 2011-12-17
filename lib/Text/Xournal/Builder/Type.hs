{-# LANGUAGE DeriveDataTypeable #-}

module Text.Xournal.Builder.Type where 

import System.Console.CmdArgs

data Xournal_builder = Test 
              deriving (Show,Data,Typeable)

test :: Xournal_builder
test = Test 

mode = modes [test]

