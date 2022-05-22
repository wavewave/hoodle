module Hoodle.Coroutine.Eraser where

import Control.Lens (over, set, view, (.~))
import Control.Monad.State
import qualified Control.Monad.State as St
import Data.Hoodle.Generic
import qualified Data.IntMap as IM
import Graphics.Hoodle.Render
import Graphics.Hoodle.Render.Type.Item
import Graphics.Hoodle.Render.Util.HitTest
import Hoodle.Accessor
import Hoodle.Coroutine.Commit
import Hoodle.Coroutine.Draw
import Hoodle.Coroutine.Pen
import Hoodle.Device
import Hoodle.ModelAction.Eraser
import Hoodle.ModelAction.Layer
import Hoodle.ModelAction.Page
import Hoodle.Type.Canvas
import Hoodle.Type.Coroutine
import Hoodle.Type.Enum
import Hoodle.Type.Event
import Hoodle.Type.HoodleState
import Hoodle.Type.PageArrangement
import Hoodle.View.Coordinate

-- |
eraserStart ::
  CanvasId ->
  PointerCoord ->
  MainCoroutine ()
eraserStart cid = commonPenStart eraserAction cid >=> const (return ())
  where
    eraserAction _cinfo pnum geometry (x, y) _ = do
      itms <- rItmsInCurrLyr
      eraserProcess cid pnum geometry itms (x, y)

-- |
eraserProcess ::
  CanvasId ->
  PageNum ->
  CanvasGeometry ->
  [RItem] ->
  (Double, Double) ->
  MainCoroutine ()
eraserProcess cid pnum geometry itms (x0, y0) = do
  r <- nextevent
  xst <- get
  forBoth' unboxBiAct (f r xst) . getCanvasInfo cid . view (unitHoodles . currentUnit) $ xst
  where
    f :: UserEvent -> HoodleState -> CanvasInfo a -> MainCoroutine ()
    f r xstate cvsInfo =
      penMoveAndUpOnly
        r
        pnum
        geometry
        defact
        (moveact xstate cvsInfo)
        upact
    defact = eraserProcess cid pnum geometry itms (x0, y0)
    upact _ = invalidateAll
    moveact xstate cvsInfo (_pcoord, (x, y)) = do
      let line = ((x0, y0), (x, y))
          hittestbbox = hltHittedByLineRough line itms
          (hittestitem, hitState) =
            St.runState (hltItmsHittedByLineFrmSelected_StateT line hittestbbox) False
      if hitState
        then do
          page <- getCurrentPageCvsId cid
          let uhdl = view (unitHoodles . currentUnit) xstate
              currhdl = unView . view hoodleModeState $ uhdl
              pgnum = view currentPageNum cvsInfo
              currlayer = getCurrentLayer page
          let (newitms, _maybebbox) = St.runState (eraseHitted hittestitem) Nothing
              newlayerbbox = set gitems newitms currlayer
          callRenderer_ $ updateLayerBuf cid newlayerbbox
          let newpagebbox = adjustCurrentLayer newlayerbbox page
              newhdlbbox = over gpages (IM.adjust (const newpagebbox) pgnum) currhdl
              newhdlmodst = ViewAppendState newhdlbbox
          uhdl' <- liftIO (updatePageAll newhdlmodst uhdl)
          commit $ (unitHoodles . currentUnit .~ ((hoodleModeState .~ newhdlmodst) uhdl')) xstate
          invalidateInBBox Nothing Efficient cid
          nitms <- rItmsInCurrLyr
          eraserProcess cid pnum geometry nitms (x, y)
        else eraserProcess cid pnum geometry itms (x, y)
