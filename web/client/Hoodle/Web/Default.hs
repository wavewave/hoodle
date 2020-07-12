{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Hoodle.Web.Default
  ( nextevent,
    sysevent,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get, put))
import Control.Monad.Trans.Crtn (request)
import Control.Monad.Trans.Crtn.Object (Arg (..), Res (..))
import Data.Foldable (for_)
import qualified Data.JSString as JSS (pack)
import Data.List (foldl')
import qualified Data.Text as T
import Hoodle.HitTest.Type (BBoxed (..))
import qualified Hoodle.Web.ForeignJS as J
import Hoodle.Web.Type.Coroutine
  ( MainCoroutine,
    MainOp (DoEvent),
  )
import Hoodle.Web.Type.Event (AllEvent (..), SystemEvent (..), UserEvent (..))
import Hoodle.Web.Type.State (DocState (..), HoodleState (..), RStroke (..))
import Hoodle.Web.Util (pathBBox, stringifyStrokeId)
import qualified JavaScript.Web.WebSocket as WS
import Message
  ( C2SMsg (SyncRequest),
    Commit (Add, Delete),
    TextSerializable (serialize),
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
  HoodleState _ _ _ sock (DocState n _) _ _ <- get
  when (s' > n) $ liftIO $ do
    let msg = SyncRequest (n, s')
    WS.send (JSS.pack . T.unpack . serialize $ msg) sock
sysevent (EDataStrokes commits) = do
  st@(HoodleState svg _ offcvs _ (DocState _ dat0) _ _) <- get
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
  put $
    st
      { _hdlstateDocState = DocState maxId dat',
        _hdlstateOverlayUpdated = True
      }
sysevent ERefresh = do
  s@(HoodleState _ cvs offcvs _ _ _ isUpdated) <- get
  when isUpdated $ do
    liftIO $ J.js_refresh cvs offcvs
    put $ s {_hdlstateOverlayUpdated = False}
