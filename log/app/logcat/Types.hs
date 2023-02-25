{-# LANGUAGE TemplateHaskell #-}

module Types
  ( LogcatState (..),
    HasLogcatState (..),
    emptyLogcatState,
  )
where

import Control.Lens (makeClassy)
import Data.Map (Map)
import qualified Data.Map as Map (empty)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq (empty)
import GHC.RTS.Events (Event (..))

data LogcatState = LogcatState
  { _logcatEventStore :: Seq Event,
    -- TODO: Queue should be a local state, not a global state, considering STM overhead.
    _logcatEventQueue :: Seq Event,
    _logcatEventHisto :: Map String Int
  }

makeClassy ''LogcatState

emptyLogcatState :: LogcatState
emptyLogcatState = LogcatState Seq.empty Seq.empty Map.empty
