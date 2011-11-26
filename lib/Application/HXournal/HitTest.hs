module Application.HXournal.HitTest where

import Data.Strict.Tuple
import Application.HXournal.Type.XournalBBox 
import Debug.Trace

-- eps = 1e-5

hitTestBBoxPoint :: BBox -> (Double,Double) -> Bool  
hitTestBBoxPoint (BBox (ulx,uly) (lrx,lry)) (x,y) 
  = ulx <= x && x <= lrx && uly <= y && y <= lry 

hitTestLineLine :: ((Double,Double),(Double,Double)) -> ((Double,Double),(Double,Double)) -> Bool 
hitTestLineLine ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) = 
    (x2-xc)*(xc-x1)>=0 && (x3-xc)*(xc-x4) >=0 
  where x21 = x2-x1 
        x43 = x4-x3 
        y21 = y2-y1 
        y43 = y4-y3
        denom = y21*x43-y43*x21 
        xc = (x21*x43*(y3-y1)+y21*x43*x1-y43*x21*x3)/(y21*x43-y43*x21)
        
hitTestLineStroke :: (IStroke a) => 
                     ((Double,Double),(Double,Double)) 
                     -> a
                     -> Bool
hitTestLineStroke line1 str = test (strokeData str) 
  where test [] = False
        test ((x:!:y):[]) = False
        test ((x0:!:y0):(x:!:y):rest) 
          = hitTestLineLine line1 ((x0,y0),(x,y))
            || test ((x:!:y) : rest)









