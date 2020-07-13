{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Hoodle.Web.Default
  ( nextevent,
    sysevent,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get), modify')
import Control.Monad.Trans.Crtn (request)
import Control.Monad.Trans.Crtn.Object (Arg (..), Res (..))
import Data.Foldable (for_)
import Data.List (foldl')
import Hoodle.HitTest.Type (BBoxed (..))
import qualified Hoodle.Web.ForeignJS as J
import Hoodle.Web.Type.Coroutine
  ( MainCoroutine,
    MainOp (DoEvent),
  )
import Hoodle.Web.Type.Event (AllEvent (..), SystemEvent (..), UserEvent (..))
import Hoodle.Web.Type.State
  ( DocState (DocState),
    RStroke (RStroke),
    docstateData,
    docstateLastCommit,
    hdlstateDocState,
    hdlstateOverlayCanvas,
    hdlstateOverlayOffCanvas,
    hdlstateOverlayUpdated,
    hdlstateSVGBox,
    hdlstateWebSocket,
  )
import Hoodle.Web.Util (pathBBox, sendBinary, stringifyStrokeId)
import Lens.Micro ((.~), (^.))
import Message
  ( C2SMsg (SyncRequest),
    Commit (Add, Delete),
    commitId,
  )

-- | Handling system events until a new user event arrives
nextevent :: MainCoroutine UserEvent
nextevent = do
  Arg DoEvent ev <- request (Res DoEvent ())
  case ev of
    SysEv sev -> sysevent sev >> nextevent
    UsrEv uev -> pure uev

-- | Handling a system event
sysevent :: SystemEvent -> MainCoroutine ()
sysevent (ERegisterStroke s') = do
  s <- get
  let sock = s ^. hdlstateWebSocket
      n = s ^. hdlstateDocState . docstateLastCommit
  when (s' > n) $ liftIO $ sendBinary sock (SyncRequest (n, s'))
sysevent (EDataStrokes commits) = do
  s <- get
  let svg = s ^. hdlstateSVGBox
      offcvs = s ^. hdlstateOverlayOffCanvas
      dat0 = s ^. hdlstateDocState . docstateData
  liftIO $ do
    J.js_clear_overlay offcvs
    for_ commits $ \case
      Add i xys -> J.drawPath svg (stringifyStrokeId i) xys
      Delete _ js -> for_ js (J.strokeRemove svg . stringifyStrokeId)
  let maxId = maximum (map commitId commits)
      dat' = foldl' f dat0 commits
        where
          f !acc (Add i xys) = acc ++ [BBoxed (RStroke i xys) (pathBBox xys)]
          f !acc (Delete _ js) = filter (\(BBoxed (RStroke j _) _) -> not (j `elem` js)) acc
  modify' ((hdlstateDocState .~ DocState maxId dat') . (hdlstateOverlayUpdated .~ True))
sysevent ERefresh = do
  s <- get
  let cvs = s ^. hdlstateOverlayCanvas
      offcvs = s ^. hdlstateOverlayOffCanvas
  when (s ^. hdlstateOverlayUpdated) $ do
    liftIO $ J.js_refresh cvs offcvs
    modify' (hdlstateOverlayUpdated .~ False)
