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


data HubInfo = HubInfo { googleClientId :: Text
                       , googleClientSecret :: Text
                       , googleAuthURL :: String
                       , hubURL :: String
                       , hubFileRoot :: FilePath
                       , hubSocketURL :: String
                       , hubSocketPort :: Int
                       , hubSocketPath :: String
                       } 
             deriving (Show,Eq,Ord)
