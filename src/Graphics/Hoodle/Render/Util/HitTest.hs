{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Util.HitTest 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Hit-testing routines
-- 
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Util.HitTest where


import Control.Applicative
import Control.Monad.State
import Data.Strict.Tuple
-- from hoodle-platform
import Data.Hoodle.Simple 
import Data.Hoodle.BBox 
-- from this package
import Graphics.Hoodle.Render.Type.HitTest
import Graphics.Hoodle.Render.Type.Item 

--------------------------
-- hit test collections --
--------------------------

-- | hit test of whether a point in a bbox
--   previously, hitTestBBoxPoint 
isPointInBBox :: BBox  
              -> (Double,Double)  -- ^ point 
              -> Bool  
isPointInBBox (BBox (ulx,uly) (lrx,lry)) (x,y) 
  = ulx <= x && x <= lrx && uly <= y && y <= lry 

-- | hit test of whether two lines intersect
do2LinesIntersect :: ((Double,Double),(Double,Double)) -- ^ line1
                  -> ((Double,Double),(Double,Double)) -- ^ line2 
                  -> Bool 
do2LinesIntersect ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) = 
    (x2-xc)*(xc-x1)>=0 && (x3-xc)*(xc-x4) >=0 
  where x21 = x2-x1 
        x43 = x4-x3 
        y21 = y2-y1 
        y43 = y4-y3
        xc = (x21*x43*(y3-y1)+y21*x43*x1-y43*x21*x3)/(y21*x43-y43*x21)
        
-- | previously, hitTestLineStroke
doesLineHitStrk :: ((Double,Double),(Double,Double)) -> Stroke -> Bool
doesLineHitStrk line1 str@(Stroke _t _c _w _d) = 
    test (stroke_data str) 
  where test [] = False
        test ((_:!:_):[]) = False
        test ((x0:!:y0):(x:!:y):rest) 
          = do2LinesIntersect line1 ((x0,y0),(x,y))
            || test ((x:!:y) : rest)
doesLineHitStrk line1 (VWStroke _t _c d) = test d 
  where test [] = False
        test ((_,_,_):[]) = False
        test ((x0,y0,_):(x,y,z):rest) 
          = do2LinesIntersect line1 ((x0,y0),(x,y))
            || test ((x,y,z) : rest)

-- | Do two bounding boxes intersect with each other?
--   previously, hitTestBBoxBBox
do2BBoxIntersect :: BBox -> BBox -> Bool  
do2BBoxIntersect b1@(BBox (ulx1,uly1) (lrx1,lry1)) b2@(BBox (ulx2,uly2) (lrx2,lry2))
  = isPointInBBox b2 (ulx1,uly1)
    || isPointInBBox b2 (lrx1,lry1)
    || isPointInBBox b2 (lrx1,uly1)
    || isPointInBBox b2 (ulx1,lry1)
    || isPointInBBox b1 (lrx2,lry2)
    || isPointInBBox b1 (ulx2,uly2)

-- | is the second bbox inside the first bbox?
--   previously, hitTestInsideBBox
isBBox2InBBox1 :: BBox -- ^ 1st bbox
               -> BBox -- ^ 2nd bbox 
               -> Bool 
isBBox2InBBox1 b1 (BBox (ulx2,uly2) (lrx2,lry2)) 
  = isPointInBBox b1 (ulx2,uly2) && isPointInBBox b1 (lrx2,lry2)

--------------------------------------------------------
-- item filtering functions that results in AlterList -- 
--------------------------------------------------------

-- | 
hltFilteredBy_StateT :: (a -> Bool)  -- ^ hit test condition
                     -> [a]          -- ^ strokes to test 
                     -> State Bool (AlterList (NotHitted a) (Hitted a))
hltFilteredBy_StateT test itms = do 
    let (nhit,rest) = break test itms
        (hit,rest') = break (not.test) rest 
    modify (|| (not.null) hit) 
    if null rest' 
      then return (NotHitted nhit :- Hitted hit :- NotHitted [] :- Empty)
      else return (NotHitted nhit :- Hitted hit :- hltFilteredBy test rest')


-- | highlight strokes filtered by a condition. 
--   previously mkHitTestAL
hltFilteredBy :: (a -> Bool)  -- ^ hit test condition
              -> [a]          -- ^ strokes to test 
              -> AlterList (NotHitted a) (Hitted a)
hltFilteredBy test is = evalState (hltFilteredBy_StateT test is) False



-- | 
hltHittedByBBox :: (BBoxable a) => 
                   BBox  -- ^ test bounding box
                -> [a] -- ^ items to test
                -> AlterList (NotHitted a) (Hitted a)
hltHittedByBBox b = hltFilteredBy (do2BBoxIntersect b . getBBox) 

-- | 
hltEmbeddedByBBox :: (BBoxable a) =>
                     BBox 
                  -> [a] 
                  -> AlterList (NotHitted a) (Hitted a)
hltEmbeddedByBBox b = hltFilteredBy (isBBox2InBBox1 b . getBBox)

-- | only check if a line and bbox of item overlapped 
hltHittedByLineRough :: (BBoxable a) => 
                        ((Double,Double),(Double,Double)) -- ^ line 
                     -> [a] -- ^ items to test
                     -> AlterList (NotHitted a) (Hitted a)
hltHittedByLineRough (p1,p2) = hltFilteredBy boxhittest 
  where boxhittest s = isPointInBBox (getBBox s) p1
                       || isPointInBBox (getBBox s) p2


-- |
hltItmsHittedByLine_StateT :: ((Double,Double),(Double,Double))
                           -> [RItem]
                           -> State Bool RItemHitted
                              -- (AlterList (NotHitted RItem) (Hitted RItem))
hltItmsHittedByLine_StateT line = hltFilteredBy_StateT test  
  where test (RItemStroke strk) = (doesLineHitStrk line . strkbbx_strk) strk
        test _ = False 
  


-- |
hltItmsHittedByLineFrmSelected_StateT :: 
  ((Double,Double),(Double,Double))
  -> RItemHitted -- AlterList (NotHitted RItem) (Hitted RItem)
  -> State Bool (AlterList (NotHitted RItem) RItemHitted)
hltItmsHittedByLineFrmSelected_StateT _ Empty 
  = error "something is wrong, invariant broken"
hltItmsHittedByLineFrmSelected_StateT _ (n:-Empty) = return (n:-Empty)
hltItmsHittedByLineFrmSelected_StateT line (n:-h:-rest) = do 
  h' <- hltItmsHittedByLine_StateT line (unHitted h)
  (n:-) . (h':-) <$> hltItmsHittedByLineFrmSelected_StateT line rest



---------------------------------------------
-- stroke filtering functions as AlterList --
---------------------------------------------
 
{-  
-- | previously mkHitTestALState
hltStrksFilteredBy_StateT :: (StrokeBBox -> Bool)  -- ^ hit test condition
                          -> [StrokeBBox]          -- ^ strokes to test 
                          -> State Bool (AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox))
hltStrksFilteredBy_StateT test strs = do 
    let (nhit,rest) = break test  strs
        (hit,rest') = break (not.test) rest 
    modify (|| (not.null) hit) 
    if null rest' 
      then return (NotHitted nhit :- Hitted hit :- NotHitted [] :- Empty)
      else return (NotHitted nhit :- Hitted hit :- hltStrksFilteredBy test rest')

-- | highlight strokes filtered by a condition. 
--   previously mkHitTestAL
hltStrksFilteredBy :: (StrokeBBox -> Bool)  -- ^ hit test condition
                   -> [StrokeBBox]          -- ^ strokes to test 
                   -> AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox)
hltStrksFilteredBy test strs = evalState (hltStrksFilteredBy_StateT test strs) False



-- | 
hltStrksHittedByBBox :: BBox  -- ^ test bounding box
                     -> [StrokeBBox] -- ^ strokes to test
                     -> AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox)
hltStrksHittedByBBox b = hltStrksFilteredBy (do2BBoxIntersect b . strkbbx_bbx) 

-- | previously, mkHitTestInsideBBox
hltStrksEmbeddedByBBox :: BBox 
                       -> [StrokeBBox] 
                       -> AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox)
hltStrksEmbeddedByBBox b = hltStrksFilteredBy (isBBox2InBBox1 b . strkbbx_bbx)

-- | only check if a line and bbox of strokebbox overlapped 
--   previously, mkHitTestBBox
hltStrksHittedByLineRough :: ((Double,Double),(Double,Double)) -- ^ line 
                          -> [StrokeBBox]                      -- ^ strokes to test
                          -> AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox)
hltStrksHittedByLineRough (p1,p2) = hltStrksFilteredBy boxhittest 
  where boxhittest s = isPointInBBox (strkbbx_bbx s) p1
                       || isPointInBBox (strkbbx_bbx s) p2



-- |
mkHitTestStroke :: ((Double,Double),(Double,Double))
                -> [StrokeBBox]
                -> State Bool (AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox))
mkHitTestStroke line = hltStrksFilteredBy_StateT (doesLineHitStrk line . strkbbx_strk)
  


-- |
hitTestStrokes :: ((Double,Double),(Double,Double))
               -> AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox)
               -> State Bool (AlterList (NotHitted StrokeBBox) StrokeHitted)
hitTestStrokes _ Empty = error "something is wrong, invariant broken"
hitTestStrokes _ (n:-Empty) = return (n:-Empty)
hitTestStrokes line (n:-h:-rest) = do 
  h' <- mkHitTestStroke line (unHitted h)
  (n:-) . (h':-) <$> hitTestStrokes line rest

-}


-- | 
elimHitted :: AlterList (NotHitted StrokeBBox) (Hitted StrokeBBox) -> State (Maybe BBox) [StrokeBBox]
elimHitted Empty = error "something wrong in elimHitted"
elimHitted (n:-Empty) = return (unNotHitted n)
elimHitted (n:-h:-rest) = do  
  bbox <- get
  let bbox2 = getTotalBBox (unHitted h) 
  put (merge bbox bbox2) 
  return . (unNotHitted n ++) =<< elimHitted rest


-- | 
merge :: Maybe BBox -> Maybe BBox -> Maybe BBox    
merge Nothing Nothing = Nothing
merge Nothing (Just b) = Just b
merge (Just b) Nothing = Just b 
merge (Just (BBox (x1,y1) (x2,y2))) (Just (BBox (x3,y3) (x4,y4))) 
  = Just (BBox (min x1 x3, min y1 y3) (max x2 x4,max y2 y4))  
    
-- | 
getTotalBBox :: [StrokeBBox] -> Maybe BBox 
getTotalBBox = foldl f Nothing 
  where f acc = merge acc . Just . strkbbx_bbx

