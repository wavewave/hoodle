{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hoodle.Coroutine.Select.ManipulateImage where

import Control.Lens (set, view, (.~), _2)
import Control.Monad (when)
import Control.Monad.State (get)
import Control.Monad.Trans (liftIO)
import Data.ByteString.Base64 (encode)
import Data.Foldable (forM_)
import Data.Hoodle.BBox
  ( BBox (..),
    BBoxed (..),
    getBBox,
  )
import Data.Hoodle.Simple
  ( Dimension (..),
    Image (..),
    Item (..),
  )
import Data.Time (UTCTime, getCurrentTime)
import qualified Graphics.GD.ByteString as G
import Graphics.Hoodle.Render.Item (cnstrctRItem, getByteStringIfEmbeddedPNG)
import Graphics.Hoodle.Render.Util.HitTest (isBBox2InBBox1)
import qualified Graphics.Rendering.Cairo as Cairo
import Hoodle.Accessor
  ( getCurrentPageEitherFromHoodleModeState,
    getGeometry4CurrCvs,
  )
import Hoodle.Coroutine.Commit (commit)
import Hoodle.Coroutine.Draw
  ( callRenderer,
    invalidateAllInBBox,
    invalidateTemp,
    nextevent,
    waitSomeEvent,
  )
import Hoodle.Coroutine.Pen (createTempRender, penMoveAndUpOnly)
import Hoodle.Coroutine.Select.Clipboard (updateTempHoodleSelectM)
import Hoodle.Device (PointerCoord)
import Hoodle.ModelAction.Page (updatePageAll)
import Hoodle.ModelAction.Pen (TempRender (..))
import Hoodle.ModelAction.Select (getNewCoordTime)
import Hoodle.ModelAction.Select.Transform (replaceSelection)
import qualified Hoodle.Type.Alias as A
import Hoodle.Type.Canvas
  ( CanvasId,
    currentPageNum,
    forBoth',
    unboxBiAct,
  )
import Hoodle.Type.Coroutine (MainCoroutine)
import Hoodle.Type.Enum
  ( DrawFlag (Efficient),
    RotateDir (CCW, CW),
  )
import Hoodle.Type.Event
  ( RenderEvent (..),
    UserEvent (..),
  )
import Hoodle.Type.HoodleState
  ( HoodleModeState (SelectState, ViewAppendState),
    currentCanvas,
    currentUnit,
    getCurrentCanvasId,
    hoodleModeState,
    unitHoodles,
  )
import Hoodle.Type.PageArrangement
  ( PageCoordinate (..),
    PageNum (..),
  )
import Hoodle.View.Coordinate
  ( CanvasGeometry,
    desktop2Page,
    device2Desktop,
  )
import Hoodle.View.Draw (renderBoxSelection)

cropImage :: BBoxed Image -> MainCoroutine ()
cropImage imgbbx = do
  xst <- get
  let uhdl = view (unitHoodles . currentUnit) xst
      (cid, cinfobox) = view currentCanvas uhdl
      hdlmodst = view hoodleModeState uhdl
      epage = forBoth' unboxBiAct (`getCurrentPageEitherFromHoodleModeState` hdlmodst) cinfobox
  case hdlmodst of
    ViewAppendState _ -> return ()
    SelectState thdl -> do
      case epage of
        Left _ -> return ()
        Right tpage -> initCropImage cid (thdl, tpage)
  where
    initCropImage cid (thdl, tpage) = do
      r <- waitSomeEvent (\case PenDown {} -> True; _ -> False)
      case r of
        PenDown cid' _pbtn pcoord ->
          when (cid == cid') $
            startCropRect cid imgbbx (thdl, tpage) pcoord
        _ -> return ()

startCropRect ::
  CanvasId ->
  BBoxed Image ->
  (A.Hoodle A.SelectMode, A.Page A.SelectMode) ->
  PointerCoord ->
  MainCoroutine ()
startCropRect cid imgbbx (thdl, tpage) pcoord0 = do
  xst <- get
  let uhdl = view (unitHoodles . currentUnit) xst
  geometry <- liftIO $ getGeometry4CurrCvs uhdl
  forM_ ((desktop2Page geometry . device2Desktop geometry) pcoord0) $ \(p0, c0) -> do
    tsel <- createTempRender geometry (p0, BBox (unPageCoord c0) (unPageCoord c0))
    ctime <- liftIO getCurrentTime
    nbbox <- newCropRect cid geometry tsel (unPageCoord c0) (unPageCoord c0, ctime)
    Cairo.surfaceFinish (tempSurfaceSrc tsel)
    Cairo.surfaceFinish (tempSurfaceTgt tsel)
    let pnum = (fst . tempInfo) tsel
        img = bbxed_content imgbbx
        obbox = getBBox imgbbx
        cvsid = getCurrentCanvasId uhdl
    when (isBBox2InBBox1 obbox nbbox) $ do
      mimg' <- liftIO $ createCroppedImage img obbox nbbox
      forM_ mimg' $ \img' -> do
        --
        callRenderer $
          GotRItem <$> cnstrctRItem (ItemImage img')
        RenderEv (GotRItem rimg') <-
          waitSomeEvent (\case RenderEv (GotRItem _) -> True; _ -> False)
        --
        let ntpage = replaceSelection rimg' tpage
        nthdl <- updateTempHoodleSelectM cvsid thdl ntpage (unPageNum pnum)
        uhdl' <- liftIO (updatePageAll (SelectState nthdl) uhdl)
        commit $ (unitHoodles . currentUnit .~ uhdl') xst
    invalidateAllInBBox Nothing Efficient
    return ()

-- | start making a new crop rectangle
newCropRect ::
  CanvasId ->
  CanvasGeometry ->
  TempRender (PageNum, BBox) ->
  (Double, Double) ->
  ((Double, Double), UTCTime) ->
  MainCoroutine BBox
newCropRect cid geometry tsel orig (prev, otime) = do
  let pnum = (fst . tempInfo) tsel
  r <- nextevent
  penMoveAndUpOnly r pnum geometry defact moveact upact
  where
    defact = newCropRect cid geometry tsel orig (prev, otime)
    --
    moveact (_pcoord, (x, y)) = do
      (willUpdate, (ncoord, ntime)) <- liftIO $ getNewCoordTime (prev, otime) (x, y)
      if willUpdate
        then do
          let oinfo@(_, BBox xy0 _) = tempInfo tsel
              nbbox = BBox xy0 (x, y)
              ninfo = set _2 nbbox oinfo
          invalidateTemp cid (tempSurfaceSrc tsel) (renderBoxSelection nbbox)
          newCropRect cid geometry tsel {tempInfo = ninfo} orig (ncoord, ntime)
        else defact
    --
    upact _pcoord = (return . snd . tempInfo) tsel

createCroppedImage :: Image -> BBox -> BBox -> IO (Maybe Image)
createCroppedImage img (BBox (xo0, yo0) (xo1, yo1)) (BBox (xn0, yn0) (xn1, yn1)) = do
  let src = img_src img
      embed = getByteStringIfEmbeddedPNG src
  case embed of
    Nothing -> return Nothing
    Just bstr -> do
      gdimg <- G.loadPngByteString bstr
      (w, h) <- G.imageSize gdimg
      let w' = floor $ fromIntegral w * (xn1 - xn0) / (xo1 - xo0)
          h' = floor $ fromIntegral h * (yn1 - yn0) / (yo1 - yo0)
          x1 = floor $ fromIntegral w * (xn0 - xo0) / (xo1 - xo0)
          y1 = floor $ fromIntegral h * (yn0 - yo0) / (yo1 - yo0)
      ngdimg <- G.newImage (w', h')
      G.copyRegion (x1, y1) (w', h') gdimg (0, 0) ngdimg
      nbstr <- G.savePngByteString ngdimg
      let nb64str = encode nbstr
          nebdsrc = "data:image/png;base64," <> nb64str
      return . Just $ Image nebdsrc (xn0, yn0) (Dim (xn1 - xn0) (yn1 - yn0))

rotateImage :: RotateDir -> BBoxed Image -> MainCoroutine ()
rotateImage dir imgbbx = do
  xst <- get
  let uhdl = view (unitHoodles . currentUnit) xst
  let (cid, cinfobox) = view currentCanvas uhdl
      hdlmodst = view hoodleModeState uhdl
      pnum = (PageNum . forBoth' unboxBiAct (view currentPageNum)) cinfobox
      epage = forBoth' unboxBiAct (`getCurrentPageEitherFromHoodleModeState` hdlmodst) cinfobox
  case hdlmodst of
    ViewAppendState _ -> return ()
    SelectState thdl -> do
      case epage of
        Left _ -> return ()
        Right tpage -> do
          let img = bbxed_content imgbbx
          mimg' <- liftIO (createRotatedImage dir img (getBBox imgbbx))
          forM_ mimg' $ \img' -> do
            callRenderer $
              GotRItem <$> cnstrctRItem (ItemImage img')
            RenderEv (GotRItem rimg') <-
              waitSomeEvent (\case RenderEv (GotRItem _) -> True; _ -> False)
            let ntpage = replaceSelection rimg' tpage
            nthdl <- updateTempHoodleSelectM cid thdl ntpage (unPageNum pnum)
            uhdl' <- liftIO (updatePageAll (SelectState nthdl) uhdl)
            commit $ (unitHoodles . currentUnit .~ uhdl') xst
          invalidateAllInBBox Nothing Efficient
          return ()

createRotatedImage :: RotateDir -> Image -> BBox -> IO (Maybe Image)
createRotatedImage dir img (BBox (x0, y0) (x1, y1)) = do
  let src = img_src img
      embed = getByteStringIfEmbeddedPNG src
  case embed of
    Nothing -> return Nothing
    Just bstr -> do
      gdimg <- G.loadPngByteString bstr
      ngdimg <- G.rotateImage (case dir of CW -> 3; CCW -> 1) gdimg
      nbstr <- G.savePngByteString ngdimg
      let nb64str = encode nbstr
          nebdsrc = "data:image/png;base64," <> nb64str
      return . Just $ Image nebdsrc (x0, y0) (Dim (y1 - y0) (x1 - x0))
