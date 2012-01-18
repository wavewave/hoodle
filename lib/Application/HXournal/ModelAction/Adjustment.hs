
-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.ModelAction.Adjustment 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
module Application.HXournal.ModelAction.Adjustment where

import Graphics.UI.Gtk 

setAdjustments :: (Adjustment,Adjustment) 
                  -> (Double,Double) 
                  -> (Double,Double)
                  -> (Double,Double) 
                  -> (Double,Double)
                  -> IO ()
setAdjustments (hadj,vadj) (upperx,uppery) (lowerx,lowery) (valuex,valuey) (pagex,pagey) = do 
    adjustmentSetUpper hadj upperx 
    adjustmentSetUpper vadj uppery 
    adjustmentSetLower hadj lowerx
    adjustmentSetLower vadj lowery
    adjustmentSetValue hadj valuex
    adjustmentSetValue vadj valuey 
    adjustmentSetPageSize hadj pagex
    adjustmentSetPageSize vadj pagey
    

     
