module Graphics.Hoodle.Render.Primitive
  ( drawStrokeCurve,
    drawVWStrokeCurve,
  )
where

import Control.Monad
import Data.Strict.Tuple hiding (fst, snd)
import qualified Graphics.Rendering.Cairo as Cairo

------------------
-- draw utility --
------------------

-- |
drawStrokeCurve :: [Pair Double Double] -> Cairo.Render ()
drawStrokeCurve ((x0 :!: y0) : xs) = do
  x0 `seq` y0 `seq` Cairo.moveTo x0 y0
  mapM_ f xs
  where
    f (x :!: y) = x `seq` y `seq` Cairo.lineTo x y
drawStrokeCurve [] = return ()

-- |
drawVWStrokeCurve :: [(Double, Double, Double)] -> Cairo.Render ()
drawVWStrokeCurve [] = return ()
drawVWStrokeCurve (_ : []) = return ()
drawVWStrokeCurve ((xo, yo, _zo) : xs) = do
  Cairo.moveTo xo yo
  let ((xlast, ylast, _zlast) : rxs) = reverse xs
  foldM_ forward (xo, yo) xs
  foldM_ forward (xlast, ylast) rxs
  where
    turn (x, y) = (negate y, x)
    norm (x, y) = sqrt (x * x + y * y)
    (x1, y1) .-. (x0, y0) = (x1 - x0, y1 - y0)
    (x1, y1) .+. (x0, y0) = (x1 + x0, y1 + y0)
    z *. (x0, y0) = (z * x0, z * y0)
    zFactor = 0.5 -- to be tuned
    forward p0 (x, y, z) = do
      let p1 = (x, y)
          dp = p1 .-. p0
          dist = norm dp
      if dist < 0.01 -- otherwise normalisation can diverge
        then return p0
        else do
          -- shift the current position perpendicularly to the
          -- direction of movement, by an amount proportional to
          -- the pressure (z).
          let shift = turn $ ((1 / dist) * z * zFactor) *. dp
          Prelude.uncurry Cairo.lineTo $ shift .+. p1
          return p1
