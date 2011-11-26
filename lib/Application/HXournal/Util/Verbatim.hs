module Application.HXournal.Util.Verbatim where

import Language.Haskell.TH.Quote

import Language.Haskell.TH.Lib

verbatim :: QuasiQuoter
verbatim = QuasiQuoter { quoteExp = litE . stringL
                       , quotePat = undefined
                       , quoteType = undefined 
                       , quoteDec = undefined
                       }
--           , quotePat = litP . stringP
--           } 

