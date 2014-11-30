module Hoodle.Type.Hub where

import Data.Text (Text)
import System.FilePath

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
