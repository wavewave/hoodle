{-# LANGUAGE DeriveDataTypeable #-}

module Graphics.Xournal.Type.Type where 

import System.Console.CmdArgs

data Xournal_types = Test 
              deriving (Show,Data,Typeable)

test :: Xournal_types
test = Test 

mode = modes [test]

