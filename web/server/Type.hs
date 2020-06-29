{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Type where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Message (CommitId (..))

-- orphan

deriving instance FromJSON CommitId

deriving instance ToJSON CommitId

data Stroke
  = Stroke
      { strokeCommitId :: CommitId,
        strokeCoordinates :: [(Double, Double)]
      }
  deriving (Generic)

instance FromJSON Stroke

instance ToJSON Stroke

data Doc = Doc {docData :: [Stroke]}
  deriving (Generic)

instance FromJSON Doc

instance ToJSON Doc
