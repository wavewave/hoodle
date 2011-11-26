{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Application.HXournal.HitTest where

import Data.Strict.Tuple
import Text.Xournal.Type 

import Application.HXournal.Type
import Application.HXournal.Type.XournalBBox 
import Application.HXournal.Util.AlterList 

import Control.Applicative
import Control.Monad.State

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
mkHitTestAL test strs = evalState (mkHitTestALState test strs) False
{-  let (nhit,rest) = break test  strs
      (hit,rest') = break (not.test) rest 
  in if null rest' 
       then NotHitted nhit :- Hitted hit :- NotHitted [] :- Empty 
       else NotHitted nhit :- Hitted hit :- mkHitTestAL test rest' 
-}

mkHitTestALState :: (StrokeBBox -> Bool) 
                 -> [StrokeBBox]
                 -> State Bool (AlterList NotHitted Hitted)
mkHitTestALState test strs = do 
  let (nhit,rest) = break test  strs
      (hit,rest') = break (not.test) rest 
  st <- get
  put (st || (not.null) hit) 
  if null rest' 
    then return (NotHitted nhit :- Hitted hit :- NotHitted [] :- Empty)
    else return (NotHitted nhit :- Hitted hit :- mkHitTestAL test rest')


mkHitTestBBox :: ((Double,Double),(Double,Double))
                 -> [StrokeBBox]
                 -> AlterList NotHitted Hitted 
mkHitTestBBox (p1,p2) = mkHitTestAL boxhittest 
  where boxhittest s = hitTestBBoxPoint (strokebbox_bbox s) p1
                       || hitTestBBoxPoint (strokebbox_bbox s) p2

mkHitTestStroke :: ((Double,Double),(Double,Double))
                -> [StrokeBBox]
                -> State Bool (AlterList NotHitted Hitted)
mkHitTestStroke line = mkHitTestALState (hitTestLineStroke line)
  
hitTestStrokes :: ((Double,Double),(Double,Double))
               -> AlterList NotHitted Hitted 
               -> State Bool (AlterList NotHitted StrokeHitted)
hitTestStrokes _ Empty = error "something is wrong, invariant broken"
hitTestStrokes _ (n:-Empty) = return (n:-Empty)
hitTestStrokes line (n:-h:-rest) = do 
  h' <- mkHitTestStroke line (unHitted h)
  (n:-) . (h':-) <$> hitTestStrokes line rest
  
elimHitted :: AlterList NotHitted Hitted -> State (Maybe BBox) [StrokeBBox]
elimHitted (n:-Empty) = return (unNotHitted n)
elimHitted (n:-h:-rest) = do  
  bbox <- get
  let bbox2 = getTotalBBox (unHitted h) 
  put (merge bbox bbox2) 
  return . (unNotHitted n ++) =<< elimHitted rest

                 
merge :: Maybe BBox -> Maybe BBox -> Maybe BBox    
merge Nothing Nothing = Nothing
merge Nothing (Just b) = Just b
merge (Just b) Nothing = Just b 
merge (Just (BBox (x1,y1) (x2,y2))) (Just (BBox (x3,y3) (x4,y4))) 
  = Just (BBox (min x1 x3, min y1 y3) (max x2 x4,max y2 y4))  
    
getTotalBBox :: [StrokeBBox] -> Maybe BBox 
getTotalBBox = foldl f Nothing 
  where f acc = merge acc . Just . strokebbox_bbox






