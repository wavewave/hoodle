{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.ProgType 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Application.HXournal.ProgType where 

import System.Console.CmdArgs

data Hxournal = Test { xojfile :: Maybe FilePath
                     }  
              deriving (Show,Data,Typeable)

test :: Hxournal
test = Test { xojfile = def &= typ "FILENAME" &= args -- &= argPos 0 &= opt "OPTIONAL" 
            } &= auto 


mode :: Hxournal
mode = modes [test] 


