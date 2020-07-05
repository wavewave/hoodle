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
import Coroutine
  ( MainCoroutine,
    MainOp (DoEvent),
  )
import Data.Foldable (traverse_)
import qualified Data.JSString as JSS (pack)
import Data.List (foldl')
import qualified Data.Text as T
import Event (AllEvent (..), SystemEvent (..), UserEvent (..))
import qualified ForeignJS as J
import Hoodle.HitTest.Type (BBoxed (..))
import Hoodle.Web.Util (pathBBox)
import qualified JavaScript.Web.WebSocket as WS
import Message
  ( C2SMsg (SyncRequest),
    Commit (Add, Delete),
    CommitId (..),
    TextSerializable (serialize),
    commitId,
  )
import State (DocState (..), HoodleState (..), RStroke (..))

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
    traverse_
      ( \case
          Add i xys -> J.drawPath svg ("stroke" ++ show (unCommitId i)) xys
          Delete _ js -> do
            traverse_
              (J.strokeRemove svg . ("stroke" ++) . show . unCommitId)
              js
      )
      commits
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
