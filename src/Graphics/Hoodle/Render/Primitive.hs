
-----------------------------------------------------------------------------
-- |
-- Module      : Graphics.Hoodle.Render.Primitive
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- draw utility 
--
-----------------------------------------------------------------------------

module Graphics.Hoodle.Render.Primitive 
( drawStrokeCurve 
, drawVWStrokeCurve 
) where 

import           Control.Applicative
import           Control.Monad 
import           Data.Strict.Tuple hiding (fst,snd)
import           Graphics.Rendering.Cairo
-- from hoodle-platform
import           Data.Hoodle.Predefined 
-- 
-- import Prelude hiding (foldM_)

------------------
-- draw utility -- 
------------------

-- | 
drawStrokeCurve :: [Pair Double Double] -> Render ()
drawStrokeCurve ((x0 :!: y0) : xs) = do 
  x0 `seq` y0 `seq` moveTo x0 y0
  mapM_ f xs 
    where f (x :!: y) = x `seq` y `seq` lineTo x y 
drawStrokeCurve [] = return ()

-- | 
drawVWStrokeCurve :: [(Double,Double,Double)] -> Render ()
drawVWStrokeCurve [] = return ()
drawVWStrokeCurve (_:[]) = return ()
drawVWStrokeCurve ((xo,yo,_zo) : xs) = do 
    moveTo xo yo
    let ((xlast,ylast,_zlast):rxs) = reverse xs 
    foldM_ forward (xo,yo) xs 
    foldM_ backward (xlast,ylast) rxs 
  where (dx,dy) = (,) <$> fst <*> snd $ predefinedPenShapeAspectXY
        dir (x,y) = x * dy - y * dx
        forward (x0,y0) (x,y,z) = do if (dir (x-x0,y-y0) > 0) 
                                       then lineTo (x+0.5*dx*z) (y+0.5*dy*z)
                                       else lineTo (x-0.5*dx*z) (y-0.5*dy*z) 
                                     return (x,y)       
        backward (x0,y0) (x,y,z) = do if (dir (x-x0,y-y0) < 0) 
                                        then lineTo (x-0.5*dx*z) (y-0.5*dy*z)
                                        else lineTo (x+0.5*dx*z) (y+0.5*dy*z)
                                      return (x,y)
