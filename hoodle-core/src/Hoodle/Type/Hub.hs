module Hoodle.Type.Hub where

import Data.Text (Text)

data HubInfo = HubInfo { cid :: Text
                       , secret :: Text
                       , authgoogleurl :: String
                       , huburl :: String
                       } 
             deriving (Show,Eq,Ord)
