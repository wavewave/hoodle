{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Hoodle.Type.Alias where

import Data.Kind (Type)
import Graphics.Hoodle.Render.Type
  ( HHoodle,
    HPage,
    RHoodle,
    RPage,
  )

-- |
data EditMode = EditMode

-- |
data SelectMode = SelectMode

type family Hoodle a :: Type

type family Page a :: Type

type family Layer a :: Type

type instance Page EditMode = RPage

type instance Page SelectMode = HPage

type instance Hoodle EditMode = RHoodle

type instance Hoodle SelectMode = HHoodle
