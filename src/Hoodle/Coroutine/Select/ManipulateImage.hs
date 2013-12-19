{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Select.ManipulateImage
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Manipulate Image in selection
-- 
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Select.ManipulateImage where

import           Control.Lens (view)
import           Control.Monad (guard)
import           Control.Monad.Trans (liftIO)
import           Control.Monad.State (get)
import           Data.Foldable (forM_)
--
import           Data.Hoodle.BBox
import           Data.Hoodle.Simple
--
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Device
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import           Hoodle.View.Coordinate

cropImage :: BBoxed Image -> MainCoroutine ()
cropImage imgbbx = do 
    liftIO $ putStrLn "cropImage called"
    xst <- get
    initCropImage (view currentCanvas xst)
  where
    initCropImage (cid,cinfobox) = do 
      r <- waitSomeEvent (\case PenDown _ _ _ -> True; _ -> False)
      case r of
        PenDown cid' pbtn pcoord -> do 
          if (cid == cid') then startCropRect imgbbx pcoord else return ()
        _ -> return ()

startCropRect :: BBoxed Image -> PointerCoord -> MainCoroutine ()
startCropRect imgbbx pcoord0 = do
  r <- waitSomeEvent (\case PenUp _ _ -> True; _ -> False)
  case r of
    PenUp _ pcoord1 -> do 
      xst <- get
      geometry <- liftIO $ getGeometry4CurrCvs xst
      let cf = (desktop2Page geometry . device2Desktop geometry) 
          mnbbox = do (p0,c0) <- cf pcoord0
                      (p1,c1) <- cf pcoord1
                      guard (p0 == p1)
                      return (BBox (unPageCoord c0) (unPageCoord c1))
          obbox = getBBox imgbbx
      forM_ mnbbox $ \nbbox -> do 
        liftIO $ print (obbox,nbbox)
    _ -> return ()

  
  