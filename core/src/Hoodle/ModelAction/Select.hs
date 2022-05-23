{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Hoodle.ModelAction.Select where

import Control.Lens (set, view)
import Data.Algorithm.Diff as A
import qualified Data.Function as F (on)
import Data.Hoodle.BBox
import Data.Hoodle.Generic
import Data.Hoodle.Select
import Data.Hoodle.Simple hiding (Hoodle, Page)
import qualified Data.IntMap as M
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import Data.Strict.Tuple
import Data.Time.Clock
import Graphics.Hoodle.Render
import Graphics.Hoodle.Render.Type
import Graphics.Hoodle.Render.Type.HitTest
import Graphics.Hoodle.Render.Util
import Graphics.Hoodle.Render.Util.HitTest
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo.Matrix (invert, transformPoint)
import qualified Graphics.UI.Gtk as Gtk
import Hoodle.HitTest (hitLassoPoint)
import Hoodle.ModelAction.Layer
import Hoodle.ModelAction.Pen
import Hoodle.ModelAction.Select.Transform
import Hoodle.Type.Alias
import Hoodle.Type.Enum
import Hoodle.Type.HoodleState
import Hoodle.Type.PageArrangement
import Hoodle.Type.Predefined
import Hoodle.View.Coordinate

--

-- |
data Handle
  = HandleTL
  | HandleTR
  | HandleBL
  | HandleBR
  | HandleTM
  | HandleBM
  | HandleML
  | HandleMR
  deriving (Show, Eq)

-- |
scaleFromToBBox :: BBox -> BBox -> (Double, Double) -> (Double, Double)
scaleFromToBBox (BBox (ox1, oy1) (ox2, oy2)) (BBox (nx1, ny1) (nx2, ny2)) (x, y) =
  let sx = (nx2 - nx1) / (ox2 - ox1)
      sy = (ny2 - ny1) / (oy2 - oy1)
      nx = (x - ox1) * sx + nx1
      ny = (y - oy1) * sy + ny1
   in (nx, ny)

-- |
isBBoxDeltaSmallerThan :: Double -> PageNum -> CanvasGeometry -> BBox -> BBox -> Bool
isBBoxDeltaSmallerThan
  delta
  pnum
  geometry
  (BBox (x11, y11) (x12, y12))
  (BBox (x21, y21) (x22, y22)) =
    let (x11', y11') = coordtrans (x11, y11)
        (x12', y12') = coordtrans (x12, y12)
        (x21', y21') = coordtrans (x21, y21)
        (x22', y22') = coordtrans (x22, y22)
     in (x11' - x21' > (-delta) && x11' - x21' < delta)
          && (y11' - y21' > (-delta) && y11' - y21' < delta)
          && (x12' - x22' > (-delta) && x12' - x22' < delta)
          && (y11' - y21' > (-delta) && y12' - y22' < delta)
    where
      coordtrans (x, y) =
        unCvsCoord . desktop2Canvas geometry . page2Desktop geometry $
          (pnum, PageCoord (x, y))

-- |
getSelectedItms :: Page SelectMode -> [RItem]
getSelectedItms = either (const []) (concatMap unHitted . getB) . rItmsInActiveLyr

-- |
getSelectedItmsFromUnitHoodle :: UnitHoodle -> Maybe [RItem]
getSelectedItmsFromUnitHoodle uhdl =
  case view hoodleModeState uhdl of
    ViewAppendState _ -> Nothing
    SelectState thdl -> fmap (getSelectedItms . Prelude.snd) (view gselSelected thdl)

-- | start a select mode with alter list selection
makePageSelectMode ::
  -- | base page
  Page EditMode ->
  -- | current selection layer (active layer will be replaced)
  TAlterHitted RItem ->
  -- | resultant select mode page
  Page SelectMode
makePageSelectMode page alist =
  let clyr = getCurrentLayer page
      nlyr = GLayer (view gbuffer clyr) (TEitherAlterHitted (Right alist))
   in set (glayers . selectedLayer) nlyr (mkHPage page)

-- | get unselected part of page and make an ordinary page
deleteSelected :: Page SelectMode -> Page SelectMode
deleteSelected tpage =
  let activelayer = rItmsInActiveLyr tpage
      buf = view (glayers . selectedLayer . gbuffer) tpage
   in case activelayer of
        Left _ -> tpage
        Right alist ->
          let leftstrs = concat (getA alist)
              layer' = GLayer buf . TEitherAlterHitted . Left $ leftstrs
           in set (glayers . selectedLayer) layer' tpage

-- |
updateTempHoodleSelect ::
  Hoodle SelectMode ->
  Page SelectMode ->
  Int ->
  Hoodle SelectMode
updateTempHoodleSelect thdl tpage pagenum =
  let pgs = view gselAll thdl
      pgs' = M.adjust (const (hPage2RPage tpage)) pagenum pgs
   in set gselAll pgs' . set gselSelected (Just (pagenum, tpage)) $ thdl

-- |
calculateWholeBBox :: [BBoxed Stroke] -> Maybe BBox
calculateWholeBBox = toMaybe . mconcat . map (Union . Middle . getBBox)

-- |
hitInSelection :: Page SelectMode -> (Double, Double) -> Bool
hitInSelection tpage point =
  case rItmsInActiveLyr tpage of
    Left _ -> False
    Right alist ->
      let Union bboxall =
            mconcat
              . map (Union . Middle . getBBox)
              . takeHitted
              $ alist
       in case bboxall of
            Middle bbox -> isPointInBBox bbox point
            _ -> False

-- |
getULBBoxFromSelected :: Page SelectMode -> ULMaybe BBox
getULBBoxFromSelected tpage =
  case rItmsInActiveLyr tpage of
    Left _ -> Bottom
    Right alist -> bbox4All . takeHitted $ alist

-- |
hitInHandle :: Page SelectMode -> (Double, Double) -> Bool
hitInHandle tpage point =
  case getULBBoxFromSelected tpage of
    Middle bbox -> isJust (checkIfHandleGrasped bbox point)
    _ -> False

-- |
toggleCutCopyDelete :: Gtk.UIManager -> Bool -> IO ()
toggleCutCopyDelete ui b = do
  agr <-
    Gtk.uiManagerGetActionGroups ui >>= \case
      [] -> error "No action group?"
      y : _ -> return y
  Just deletea <- Gtk.actionGroupGetAction agr "DELETEA"
  Just copya <- Gtk.actionGroupGetAction agr "COPYA"
  Just cuta <- Gtk.actionGroupGetAction agr "CUTA"
  let copycutdeletea = [copya, cuta, deletea]
  mapM_ (`Gtk.actionSetSensitive` b) copycutdeletea

-- |
togglePaste :: Gtk.UIManager -> Bool -> IO ()
togglePaste ui b = do
  agr <-
    Gtk.uiManagerGetActionGroups ui >>= \case
      [] -> error "No action group?"
      y : _ -> return y
  Just pastea <- Gtk.actionGroupGetAction agr "PASTEA"
  Gtk.actionSetSensitive pastea b

-- |
changeStrokeColor :: PenColor -> BBoxed Stroke -> BBoxed Stroke
changeStrokeColor pcolor str =
  let Just cname = Map.lookup pcolor penColorNameMap
      strsmpl = bbxed_content str
   in str {bbxed_content = set color cname strsmpl}

-- |
changeStrokeWidth :: Double -> BBoxed Stroke -> BBoxed Stroke
changeStrokeWidth pwidth str =
  let nstrsmpl = case bbxed_content str of
        Stroke t c _w d -> Stroke t c pwidth d
        VWStroke t c d -> Stroke t c pwidth (map (\(x, y, _z) -> x :!: y) d)
   in -- Img b w h -> Img b w h
      str {bbxed_content = nstrsmpl}

-- |
changeItemStrokeWidth :: Double -> RItem -> RItem
changeItemStrokeWidth pwidth (RItemStroke strk) = RItemStroke (changeStrokeWidth pwidth strk)
changeItemStrokeWidth _ r = r

-- |
changeItemStrokeColor :: PenColor -> RItem -> RItem
changeItemStrokeColor pcolor (RItemStroke strk) = RItemStroke (changeStrokeColor pcolor strk)
changeItemStrokeColor _ r = r

-- |
newtype CmpBBox a = CmpBBox {unCmpBBox :: a}

-- deriving Show
instance (GetBBoxable a) => Eq (CmpBBox a) where
  CmpBBox s1 == CmpBBox s2 = getBBox s1 == getBBox s2

-- |
isSame :: Diff a -> Bool
isSame (A.Both _ _) = True
isSame _ = False

-- |
separateFS :: [Diff a] -> ([a], [a])
separateFS = foldr f ([], [])
  where
    f (A.First x) (fs, ss) = (x : fs, ss)
    f (A.Second x) (fs, ss) = (fs, x : ss)
    f (A.Both _ _) (fs, ss) = (fs, ss)

-- |
getDiffBBox :: (GetBBoxable a) => [a] -> [a] -> [Diff a]
getDiffBBox lst1 lst2 =
  let -- nlst1 = fmap CmpBBox lst1
      -- nlst2 = fmap CmpBBox lst2
      diffresult = getDiffBy ((==) `F.on` CmpBBox) lst1 lst2
   in diffresult -- map (\(x,y)->(x,unCmpBBox y)) diffresult

-- |
checkIfHandleGrasped :: BBox -> (Double, Double) -> Maybe Handle
checkIfHandleGrasped (BBox (ulx, uly) (lrx, lry)) (x, y)
  | isPointInBBox (BBox (ulx - 5, uly - 5) (ulx + 5, uly + 5)) (x, y) = Just HandleTL
  | isPointInBBox (BBox (lrx - 5, uly - 5) (lrx + 5, uly + 5)) (x, y) = Just HandleTR
  | isPointInBBox (BBox (ulx - 5, lry - 5) (ulx + 5, lry + 5)) (x, y) = Just HandleBL
  | isPointInBBox (BBox (lrx - 5, lry - 5) (lrx + 5, lry + 5)) (x, y) = Just HandleBR
  | isPointInBBox (BBox (0.5 * (ulx + lrx) - 5, uly - 5) (0.5 * (ulx + lrx) + 5, uly + 5)) (x, y) = Just HandleTM
  | isPointInBBox (BBox (0.5 * (ulx + lrx) - 5, lry - 5) (0.5 * (ulx + lrx) + 5, lry + 5)) (x, y) = Just HandleBM
  | isPointInBBox (BBox (ulx - 5, 0.5 * (uly + lry) - 5) (ulx + 5, 0.5 * (uly + lry) + 5)) (x, y) = Just HandleML
  | isPointInBBox (BBox (lrx - 5, 0.5 * (uly + lry) - 5) (lrx + 5, 0.5 * (uly + lry) + 5)) (x, y) = Just HandleMR
  | otherwise = Nothing

getNewBBoxFromHandlePos :: Handle -> BBox -> (Double, Double) -> BBox
getNewBBoxFromHandlePos handle (BBox (ox1, oy1) (ox2, oy2)) (x, y) =
  case handle of
    HandleTL -> BBox (x, y) (ox2, oy2)
    HandleTR -> BBox (ox1, y) (x, oy2)
    HandleBL -> BBox (x, oy1) (ox2, y)
    HandleBR -> BBox (ox1, oy1) (x, y)
    HandleTM -> BBox (ox1, y) (ox2, oy2)
    HandleBM -> BBox (ox1, oy1) (ox2, y)
    HandleML -> BBox (x, oy1) (ox2, oy2)
    HandleMR -> BBox (ox1, oy1) (x, oy2)

-- |
hitLassoStroke :: Seq (Double, Double) -> BBoxed Stroke -> Bool
hitLassoStroke lst = all (hitLassoPoint lst) . getXYtuples . bbxed_content

-- |
hitLassoItem :: Seq (Double, Double) -> RItem -> Bool
hitLassoItem lst (RItemStroke strk) = hitLassoStroke lst strk
hitLassoItem lst (RItemImage img _) =
  hitLassoPoint lst (x1, y1) && hitLassoPoint lst (x1, y2)
    && hitLassoPoint lst (x2, y1)
    && hitLassoPoint lst (x2, y2)
  where
    BBox (x1, y1) (x2, y2) = getBBox img
hitLassoItem lst (RItemSVG svg _) =
  hitLassoPoint lst (x1, y1) && hitLassoPoint lst (x1, y2)
    && hitLassoPoint lst (x2, y1)
    && hitLassoPoint lst (x2, y2)
  where
    BBox (x1, y1) (x2, y2) = getBBox svg
hitLassoItem lst (RItemLink lnk _) =
  hitLassoPoint lst (x1, y1) && hitLassoPoint lst (x1, y2)
    && hitLassoPoint lst (x2, y1)
    && hitLassoPoint lst (x2, y2)
  where
    BBox (x1, y1) (x2, y2) = getBBox lnk
hitLassoItem lst (RItemAnchor anc _) =
  hitLassoPoint lst (x1, y1) && hitLassoPoint lst (x1, y2)
    && hitLassoPoint lst (x2, y1)
    && hitLassoPoint lst (x2, y2)
  where
    BBox (x1, y1) (x2, y2) = getBBox anc

type TempSelection = TempRender [RItem]

data ItmsNImg = ItmsNImg
  { itmNimg_itms :: [RItem],
    itmNimg_mbbx :: Maybe BBox,
    imageSurface :: Cairo.Surface
  }

-- |
mkItmsNImg :: RenderCache -> CanvasId -> Page SelectMode -> IO ItmsNImg
mkItmsNImg cache cid tpage = do
  let itms = getSelectedItms tpage
      drawselection = mapM_ (renderRItem cache cid) itms
      Dim cw ch = view gdimension tpage
      mbbox = case getULBBoxFromSelected tpage of
        Middle bbox -> Just bbox
        _ -> Nothing
  sfc <- Cairo.createImageSurface Cairo.FormatARGB32 (floor cw) (floor ch)
  Cairo.renderWith sfc $ do
    Cairo.setSourceRGBA 1 1 1 0
    Cairo.rectangle 0 0 cw ch
    Cairo.fill
    Cairo.setSourceRGBA 0 0 0 1
    drawselection
  return $ ItmsNImg itms mbbox sfc

-- |
drawTempSelectImage ::
  CanvasGeometry ->
  TempRender ItmsNImg ->
  -- | transformation matrix
  Cairo.Matrix ->
  Cairo.Render ()
drawTempSelectImage geometry tempselection xformmat = do
  let sfc = imageSurface (tempInfo tempselection)
      CanvasDimension (Dim cw ch) = canvasDim geometry
      invxformmat = invert xformmat
      newvbbox =
        BBox
          (transformPoint invxformmat (0, 0))
          (transformPoint invxformmat (cw, ch))
      mbbox = itmNimg_mbbx (tempInfo tempselection)
      newmbbox = case unIntersect (Intersect (Middle newvbbox) `mappend` fromMaybe mbbox) of
        Middle bbox -> Just bbox
        _ -> Just newvbbox
  Cairo.setMatrix xformmat
  clipBBox newmbbox
  Cairo.setSourceSurface sfc 0 0
  Cairo.setOperator Cairo.OperatorOver
  Cairo.paint

-- |
getNewCoordTime ::
  ((Double, Double), UTCTime) ->
  (Double, Double) ->
  IO (Bool, ((Double, Double), UTCTime))
getNewCoordTime (prev, otime) (x, y) = do
  ntime <- getCurrentTime
  let dtime = diffUTCTime ntime otime
      willUpdate = dtime > dtimeBound
      (nprev, nntime) =
        if dtime > dtimeBound
          then ((x, y), ntime)
          else (prev, otime)
  return (willUpdate, (nprev, nntime))

-- |
adjustItemPosition4Paste :: CanvasGeometry -> PageNum -> [RItem] -> [RItem]
adjustItemPosition4Paste geometry pgn itms =
  case bbox of
    Middle (BBox (xs0, ys0) _) -> fmap (changeItemBy (offsetFunc (x0 - xs0, y0 - ys0))) itms
    _ -> itms
  where
    bbox = bbox4All itms
    ViewPortBBox (BBox (xv0, yv0) _) = canvasViewPort geometry
    (x0, y0) =
      maybe
        (0, 0)
        (\(pgn', norigin) -> if pgn == pgn' then unPageCoord norigin else (0, 0))
        $ desktop2Page geometry (DeskCoord (xv0, yv0))
