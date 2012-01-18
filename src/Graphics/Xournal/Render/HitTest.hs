{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Xournal.Render.HitTest 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--

module Graphics.Xournal.Render.HitTest where

import Data.Strict.Tuple
import Data.Xournal.Simple 
import Data.Xournal.BBox 
import Data.Xournal.Generic

import Graphics.Xournal.Render.Type

import Control.Applicative
import Control.Monad.State


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
        -- denom = y21*x43-y43*x21 
        xc = (x21*x43*(y3-y1)+y21*x43*x1-y43*x21*x3)/(y21*x43-y43*x21)
        
hitTestLineStroke :: ((Double,Double),(Double,Double)) 
                     -> Stroke
                     -> Bool
hitTestLineStroke line1 str = test (stroke_data str) 
  where test [] = False
        test ((_:!:_):[]) = False
        test ((x0:!:y0):(x:!:y):rest) 
          = hitTestLineLine line1 ((x0,y0),(x,y))
            || test ((x:!:y) : rest)
            
mkHitTestAL :: (StrokeBBox -> Bool) 
            -> [StrokeBBox]
            -> AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox)
mkHitTestAL test strs = evalState (mkHitTestALState test strs) False

mkHitTestALState :: (StrokeBBox -> Bool) 
                 -> [StrokeBBox]
                 -> State Bool (AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox))
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
                 -> AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox)
mkHitTestBBox (p1,p2) = mkHitTestAL boxhittest 
  where boxhittest s = hitTestBBoxPoint (strokebbox_bbox s) p1
                       || hitTestBBoxPoint (strokebbox_bbox s) p2

mkHitTestBBoxBBox :: BBox -> [StrokeBBox] -> AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox)
mkHitTestBBoxBBox b = mkHitTestAL (hitTestBBoxBBox b . strokebbox_bbox) 

mkHitTestInsideBBox :: BBox -> [StrokeBBox] -> AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox)
mkHitTestInsideBBox b = mkHitTestAL (hitTestInsideBBox b . strokebbox_bbox)

hitTestInsideBBox :: BBox -> BBox -> Bool 
hitTestInsideBBox b1 (BBox (ulx2,uly2) (lrx2,lry2)) 
  = hitTestBBoxPoint b1 (ulx2,uly2)
    && hitTestBBoxPoint b1 (lrx2,lry2)


hitTestBBoxBBox :: BBox -> BBox -> Bool  
hitTestBBoxBBox b1@(BBox (ulx1,uly1) (lrx1,lry1)) b2@(BBox (ulx2,uly2) (lrx2,lry2))
  = hitTestBBoxPoint b2 (ulx1,uly1)
    || hitTestBBoxPoint b2 (lrx1,lry1)
    || hitTestBBoxPoint b2 (lrx1,uly1)
    || hitTestBBoxPoint b2 (ulx1,lry1)
    || hitTestBBoxPoint b1 (lrx2,lry2)
    || hitTestBBoxPoint b1 (ulx2,uly2)
 


mkHitTestStroke :: ((Double,Double),(Double,Double))
                -> [StrokeBBox]
                -> State Bool (AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox))
mkHitTestStroke line = mkHitTestALState (hitTestLineStroke line . gToStroke)
  
hitTestStrokes :: ((Double,Double),(Double,Double))
               -> AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox)
               -> State Bool (AlterList (NotHitted StrokeBBox) StrokeHitted)
hitTestStrokes _ Empty = error "something is wrong, invariant broken"
hitTestStrokes _ (n:-Empty) = return (n:-Empty)
hitTestStrokes line (n:-h:-rest) = do 
  h' <- mkHitTestStroke line (unHitted h)
  (n:-) . (h':-) <$> hitTestStrokes line rest
  
elimHitted :: AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox) -> State (Maybe BBox) [StrokeBBox]
elimHitted Empty = error "something wrong in elimHitted"
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






