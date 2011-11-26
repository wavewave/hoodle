{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Application.HXournal.HitTest where

import Data.Strict.Tuple
import Application.HXournal.Type
import Application.HXournal.Type.XournalBBox 
import Application.HXournal.Util.AlterList 

import Debug.Trace

-- eps = 1e-5

newtype NotHitted = NotHitted { unNotHitted :: [StrokeBBox] } 
                  deriving (Show)

newtype Hitted = Hitted { unHitted :: [StrokeBBox] } 
                 deriving (Show)

type StrokeHitted = AlterList NotHitted Hitted 

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
            
mkHitTestAL :: (StrokeBBox -> Bool) 
            -> [StrokeBBox]
            -> AlterList NotHitted Hitted 
mkHitTestAL test strs = 
  let (nhit,rest) = break test  strs
      (hit,rest') = break (not.test) rest 
  in if null rest' 
       then NotHitted nhit :- Hitted hit :- NotHitted [] :- Empty 
       else NotHitted nhit :- Hitted hit :- mkHitTestAL test rest' 

mkHitTestBBox :: ((Double,Double),(Double,Double))
                 -> [StrokeBBox]
                 -> AlterList NotHitted Hitted 
mkHitTestBBox (p1,p2) = mkHitTestAL boxhittest 
  where boxhittest s = hitTestBBoxPoint (strokebbox_bbox s) p1
                       || hitTestBBoxPoint (strokebbox_bbox s) p2

mkHitTestStroke :: ((Double,Double),(Double,Double))
                -> [StrokeBBox]
                -> AlterList NotHitted Hitted  
mkHitTestStroke line = mkHitTestAL (hitTestLineStroke line)
  
hitTestStrokes :: ((Double,Double),(Double,Double))
               -> AlterList NotHitted Hitted 
               -> AlterList NotHitted StrokeHitted
hitTestStrokes _ Empty = error "something is wrong, invariant broken"
hitTestStrokes _ (n:-Empty) = n:-Empty
hitTestStrokes line (n:-h:-rest) =
  let h' = mkHitTestStroke line (unHitted h)
  in  n:-h':-(hitTestStrokes line rest)
  

                 






