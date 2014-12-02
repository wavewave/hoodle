-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Hub
-- Copyright   : (c) 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Type.Hub where

import Data.Text (Text)
--


data HubInfo = HubInfo { cid :: Text
                       , secret :: Text
                       , authgoogleurl :: String
                       , hubfileurl :: String
                       , hubfileroot :: FilePath
                       , hubsocketurl :: String
                       , hubsocketport :: Int
                       , hubsocketpath :: String
                       } 
             deriving (Show,Eq,Ord)
