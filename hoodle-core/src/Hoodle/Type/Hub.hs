module Hoodle.Type.Hub where

import Data.Text (Text)
import System.FilePath

data HubInfo = HubInfo { cid :: Text
                       , secret :: Text
                       , authgoogleurl :: String
                       , huburl :: String
                       , hubfileroot :: FilePath
                       } 
             deriving (Show,Eq,Ord)
