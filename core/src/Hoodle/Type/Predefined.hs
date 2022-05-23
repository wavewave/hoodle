module Hoodle.Type.Predefined where

import Data.Time.Clock

-- |
millisec :: Integer
millisec = 1000000000

dtimeBound :: NominalDiffTime
dtimeBound = realToFrac (picosecondsToDiffTime (50 * millisec))

-- |
predefinedWinReconfTimeBound :: NominalDiffTime
predefinedWinReconfTimeBound = realToFrac (picosecondsToDiffTime 50000000000)

-- |
predefinedLassoColor :: (Double, Double, Double, Double)
predefinedLassoColor = (1.0, 116.0 / 255.0, 0, 0.8)

-- |
predefinedLassoWidth :: Double
predefinedLassoWidth = 1.0

-- |
predefinedLassoHandleSize :: Double
predefinedLassoHandleSize = 4.0

-- |
predefinedLassoDash :: ([Double], Double)
predefinedLassoDash = ([2, 2], 4)

-- |
predefinedPageSpacing :: Double
predefinedPageSpacing = 10

-- |
predefinedZoomStepFactor :: Double
predefinedZoomStepFactor = 1.10

-- |
maxCursorWidth :: Int
maxCursorWidth = 30

-- |
maxCursorHeight :: Int
maxCursorHeight = 30
