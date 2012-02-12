-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Type.Predefined
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
----------------------------------------------------------------------------

module Application.HXournal.Type.Predefined where

import Data.Time.Clock 

predefinedPenShapeAspectXY :: (Double,Double)
predefinedPenShapeAspectXY = (cos (pi/6.0), sin (pi/6.0))


dtime_bound :: NominalDiffTime 
dtime_bound = realToFrac (picosecondsToDiffTime 100000000000)

predefinedWinReconfTimeBound :: NominalDiffTime
predefinedWinReconfTimeBound = realToFrac (picosecondsToDiffTime 50000000000)

predefinedLassoColor :: (Double,Double,Double,Double)
predefinedLassoColor = (1.0,116.0/255.0,0,0.8)

predefinedLassoWidth :: Double 
predefinedLassoWidth = 4.0

predefinedLassoDash :: ([Double],Double)
predefinedLassoDash = ([10,5],10) 

predefinedPageSpacing :: Double
predefinedPageSpacing = 10 
