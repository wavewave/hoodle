module Hoodle.Type.Hub where

import Data.Text (Text)

--

data HubInfo
  = HubInfo
      { googleClientId :: Text,
        googleClientSecret :: Text,
        googleAuthURL :: String,
        hubURL :: String,
        hubFileRoot :: FilePath,
        hubSocketURL :: String,
        hubSocketPort :: Int,
        hubSocketPath :: String
      }
  deriving (Show, Eq, Ord)
