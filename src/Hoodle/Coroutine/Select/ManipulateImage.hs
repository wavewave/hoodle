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

import           Control.Lens (set, view, _2)
import           Control.Monad (guard, when)
import           Control.Monad.Trans (liftIO)
import           Control.Monad.State (get)
import           Data.Foldable (forM_)
import           Data.Time
import           Graphics.Rendering.Cairo
--
import           Data.Hoodle.BBox
import           Data.Hoodle.Simple
--
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Pen
import           Hoodle.Device
import           Hoodle.ModelAction.Pen
import           Hoodle.ModelAction.Select
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import           Hoodle.View.Coordinate
import           Hoodle.View.Draw
--

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
          if (cid == cid') then startCropRect cid imgbbx pcoord else return ()
        _ -> return ()

startCropRect :: CanvasId -> BBoxed Image -> PointerCoord -> MainCoroutine ()
startCropRect cid imgbbx pcoord0 = do
    xst <- get
    geometry <- liftIO $ getGeometry4CurrCvs xst
    forM_ ((desktop2Page geometry . device2Desktop geometry) pcoord0) $ \(p0,c0) -> do
      tsel <- createTempRender geometry (p0, BBox (unPageCoord c0) (unPageCoord c0))
      ctime <- liftIO $ getCurrentTime
      newCropRect cid geometry tsel (unPageCoord c0) (unPageCoord c0,ctime)
      surfaceFinish (tempSurfaceSrc tsel)
      surfaceFinish (tempSurfaceTgt tsel)
      invalidateAllInBBox Nothing Efficient      
      return ()
    
      
newCropRect :: CanvasId 
            -> CanvasGeometry 
            -> TempRender (PageNum,BBox) 
            -> (Double,Double) 
            -> ((Double,Double),UTCTime) 
            -> MainCoroutine BBox
newCropRect cid geometry tsel orig (prev,otime) = do
    let pnum = (fst . tempInfo) tsel
    r <- nextevent 
    penMoveAndUpOnly r pnum geometry defact moveact upact
  where 
    defact = newCropRect cid geometry tsel orig (prev,otime)
    -- 
    moveact (pcoord,(x,y)) = do 
      (willUpdate,(ncoord,ntime)) <- liftIO $ getNewCoordTime (prev,otime) (x,y)
      if willUpdate 
        then do 
          let oinfo@(_,BBox xy0 _) = tempInfo tsel 
              nbbox = BBox xy0 (x,y)
              ninfo = set _2 nbbox oinfo  
          invalidateTemp cid (tempSurfaceSrc tsel) (renderBoxSelection nbbox)
          newCropRect cid geometry tsel {tempInfo = ninfo} orig (ncoord,ntime)
        else defact
    -- 
    upact pcoord = do
      (return . snd . tempInfo) tsel

{-       
      
        let oinfo@(p0,BBox xy0 oxy1) = tempInfo tsel
            ninfo = maybe oinfo id $ do 
                      (p1,c1) <- cf pcoord1
                      guard (p0 == p1)
                      return (p0,BBox xy0 (unPageCoord c1)) 
        liftIO (print ninfo)
        newCropRect geometry tsel {tempInfo = ninfo} pcoord0 
      PenUp _ pcoord1 -> do 
        --    mnbbox = do (p0,c0) <- cf pcoord0
        --                (p1,c1) <- cf pcoord1
        --                guard (p0 == p1)
        --                return (BBox (unPageCoord c0) (unPageCoord c1))
        --    obbox = getBBox imgbbx
        -- forM_ mnbbox $ \nbbox -> do 
        --   liftIO $ print (obbox,nbbox)
        (return . snd . tempInfo) tsel
      _ -> newCropRect geometry tsel pcoord0
  where 
    cf = (desktop2Page geometry . device2Desktop geometry) 
-}