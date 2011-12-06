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
    

     