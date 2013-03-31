-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Util.Verbatim 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Util.Verbatim where

import Language.Haskell.TH.Quote

import Language.Haskell.TH.Lib

verbatim :: QuasiQuoter
verbatim = QuasiQuoter { quoteExp = litE . stringL
                       , quotePat = undefined
                       , quoteType = undefined 
                       , quoteDec = undefined
                       }

