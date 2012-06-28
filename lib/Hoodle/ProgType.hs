{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.ProgType 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Hoodle.ProgType where 

import System.Console.CmdArgs

data Hoodle = Test { xojfile :: Maybe FilePath
                     }  
              deriving (Show,Data,Typeable)

test :: Hoodle
test = Test { xojfile = def &= typ "FILENAME" &= args -- &= argPos 0 &= opt "OPTIONAL" 
            } &= auto 


mode :: Hoodle
mode = modes [test] 


