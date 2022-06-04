module Hoodle.Coroutine.Eraser where

import Control.Lens (over, set, view, (.~))
import Control.Monad ((>=>))
import Control.Monad.State (get, liftIO)
import qualified Control.Monad.State as St
import Data.Hoodle.Generic
  ( gitems,
    gpages,
  )
import qualified Data.IntMap as IM
import Graphics.Hoodle.Render (updateLayerBuf)
import Graphics.Hoodle.Render.Type.Item (RItem)
import Graphics.Hoodle.Render.Util.HitTest
  ( hltHittedByLineRough,
    hltItmsHittedByLineFrmSelectedStateT,
  )
import Hoodle.Accessor
  ( getCurrentPageCvsId,
    rItmsInCurrLyr,
  )
import Hoodle.Coroutine.Commit (commit)
import Hoodle.Coroutine.Draw
  ( callRenderer_,
    invalidateAll,
    invalidateInBBox,
    nextevent,
  )
import Hoodle.Coroutine.Pen
  ( commonPenStart,
    penMoveAndUpOnly,
  )
import Hoodle.Device (PointerCoord)
import Hoodle.ModelAction.Eraser (eraseHitted)
import Hoodle.ModelAction.Layer
  ( adjustCurrentLayer,
    getCurrentLayer,
  )
import Hoodle.ModelAction.Page (updatePageAll)
import Hoodle.Type.Canvas
  ( CanvasId,
    CanvasInfo (..),
    currentPageNum,
    forBoth',
    unboxBiAct,
  )
import Hoodle.Type.Coroutine (MainCoroutine)
import Hoodle.Type.Enum (DrawFlag (Efficient))
import Hoodle.Type.Event (UserEvent)
import Hoodle.Type.HoodleState
  ( HoodleModeState (ViewAppendState, unView),
    HoodleState,
    currentUnit,
    getCanvasInfo,
    hoodleModeState,
    unitHoodles,
  )
import Hoodle.Type.PageArrangement (PageNum (..))
import Hoodle.View.Coordinate (CanvasGeometry)

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
            St.runState (hltItmsHittedByLineFrmSelectedStateT line hittestbbox) False
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
          commit $ (unitHoodles . currentUnit .~ (hoodleModeState .~ newhdlmodst) uhdl') xstate
          invalidateInBBox Nothing Efficient cid
          nitms <- rItmsInCurrLyr
          eraserProcess cid pnum geometry nitms (x, y)
        else eraserProcess cid pnum geometry itms (x, y)
