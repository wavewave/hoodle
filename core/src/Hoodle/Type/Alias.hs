{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Hoodle.Type.Alias where

import Graphics.Hoodle.Render.Type

-- |
data EditMode = EditMode

-- |
data SelectMode = SelectMode

type family Hoodle a :: *

type family Page a :: *

type family Layer a :: *

-- type instance Layer EditMode = RLayer
-- type instance Layer SelectMode = HLayers

type instance Page EditMode = RPage

type instance Page SelectMode = HPage

type instance Hoodle EditMode = RHoodle

type instance Hoodle SelectMode = HHoodle
