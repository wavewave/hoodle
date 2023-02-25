{-# LANGUAGE TemplateHaskell #-}

module Types
  ( -- * view state
    ViewState (..),
    HasViewState (..),
    emptyViewState,

    -- * top-level logcat state
    LogcatState (..),
    HasLogcatState (..),
    emptyLogcatState,
  )
where

import Control.Lens (makeClassy)
import Data.Fixed (Nano)
import Data.Map (Map)
import qualified Data.Map as Map (empty)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq (empty)
import GHC.RTS.Events (Event (..))

data ViewState = ViewState
  { -- | start point of the timeline view
    _viewTimeOrigin :: Nano
  }

makeClassy ''ViewState

emptyViewState :: ViewState
emptyViewState = ViewState 0

data LogcatState = LogcatState
  { _logcatEventStore :: Seq Event,
    -- TODO: Queue should be a local state, not a global state, considering STM overhead.
    _logcatEventQueue :: Seq Event,
    _logcatEventHisto :: Map String Int,
    _logcatLastEventTime :: Nano,
    _logcatViewState :: ViewState
  }

makeClassy ''LogcatState

emptyLogcatState :: LogcatState
emptyLogcatState =
  LogcatState
    { _logcatEventStore = Seq.empty,
      _logcatEventQueue = Seq.empty,
      _logcatEventHisto = Map.empty,
      _logcatLastEventTime = 0,
      _logcatViewState = emptyViewState
    }
