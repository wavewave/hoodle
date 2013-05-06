-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Predefined
-- Copyright   : (c) 2012, 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
----------------------------------------------------------------------------

module Hoodle.Type.Predefined where

import Data.Time.Clock 

-- | 

millisec = 1000000000

dtime_bound :: NominalDiffTime 
dtime_bound = realToFrac (picosecondsToDiffTime (25*millisec))
  -- realToFrac (picosecondsToDiffTime 100000000000)

-- | 

predefinedWinReconfTimeBound :: NominalDiffTime
predefinedWinReconfTimeBound = realToFrac (picosecondsToDiffTime 50000000000)

-- |

predefinedLassoColor :: (Double,Double,Double,Double)
predefinedLassoColor = (1.0,116.0/255.0,0,0.8)

-- | 
predefinedLassoWidth :: Double 
predefinedLassoWidth = 1.0

-- | 
predefinedLassoHandleSize :: Double 
predefinedLassoHandleSize = 4.0 


-- | 

predefinedLassoDash :: ([Double],Double)
predefinedLassoDash = ([2,2],4) 

-- | 

predefinedPageSpacing :: Double
predefinedPageSpacing = 10 

-- | 

predefinedZoomStepFactor :: Double
predefinedZoomStepFactor = 1.10

