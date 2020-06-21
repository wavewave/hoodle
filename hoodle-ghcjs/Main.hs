{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get, put), modify')
import Control.Monad.Trans.Crtn (request)
import qualified Control.Monad.Trans.Crtn.Driver as D (driver)
import Control.Monad.Trans.Crtn.Object (Arg (..), Res (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Coroutine
  ( EventVar,
    MainCoroutine,
    MainObj,
    MainOp (DoEvent),
    putStrLnAndFlush,
    simplelogger,
    world,
  )
import Data.Foldable (toList, traverse_)
import Data.Hashable (hash)
import qualified Data.JSString as JSS (pack)
import Data.List (foldl', nub, sort)
import Data.Sequence (Seq, ViewR (..), singleton, viewr, (|>))
import qualified Data.Text as T
import Event (AllEvent (..), SystemEvent (..), UserEvent (..))
import qualified ForeignJS as J
import GHCJS.Marshal (FromJSVal (..), ToJSVal (..))
import GHCJS.Types (JSVal)
import Handler (setupCallback)
import HitTest (doesLineHitStrk)
import qualified JavaScript.Web.WebSocket as WS
import Message
  ( C2SMsg (DeleteStrokes, NewStroke, SyncRequest),
    Commit (Add, Delete),
    CommitId (..),
    TextSerializable (serialize),
    commitId,
  )
import State (DocState (..), HoodleState (..), SyncState (..))

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
  let i = maximum (map commitId commits)
      dat' = foldl' f dat0 commits
        where
          f !acc (Add i xys) = acc ++ [(i, xys)]
          f !acc (Delete i js) = filter (\(j, _) -> not (j `elem` js)) acc
  put $
    st
      { _hdlstateDocState = DocState i dat',
        _hdlstateOverlayUpdated = True
      }
sysevent ERefresh = do
  s@(HoodleState _ cvs offcvs _ _ _ isUpdated) <- get
  when isUpdated $ do
    liftIO $ J.js_refresh cvs offcvs
    liftIO $ putStrLnAndFlush "refresh"
    put $ s {_hdlstateOverlayUpdated = False}

guiProcess :: AllEvent -> MainCoroutine ()
guiProcess (SysEv sev) = sysevent sev >> nextevent >>= penReady
guiProcess (UsrEv uev) = penReady uev

penReady :: UserEvent -> MainCoroutine ()
penReady ev = do
  case ev of
    PointerDown (x, y) ->
      drawingMode (singleton (x, y))
    ToPenMode -> pure ()
    ToEraserMode -> nextevent >>= eraserReady
    _ -> pure ()
  nextevent >>= penReady

eraserReady :: UserEvent -> MainCoroutine ()
eraserReady ev = do
  case ev of
    ToPenMode -> nextevent >>= penReady
    PointerDown (x0, y0) -> do
      HoodleState svg _ _ _ _ _ _ <- get
      (x, y) <- liftIO $ J.getXYinSVG svg (x0, y0)
      erasingMode [] (x, y)
    _ -> pure ()
  nextevent >>= eraserReady

drawingMode :: Seq (Double, Double) -> MainCoroutine ()
drawingMode xys = do
  ev <- nextevent
  case ev of
    PointerMove xy@(x, y) -> do
      s@(HoodleState _ cvs offcvs _ _ _ _) <- get
      case viewr xys of
        _ :> (x0, y0) -> liftIO $ J.js_overlay_point cvs offcvs x0 y0 x y
        _ -> pure ()
      put $ s {_hdlstateOverlayUpdated = True}
      drawingMode (xys |> xy)
    PointerUp xy -> do
      HoodleState svg _ _ sock _ _ _ <- get
      let xys' = xys |> xy
      path_arr <-
        liftIO $
          J.js_to_svg_point_array svg =<< toJSValListOf (toList xys')
      path <- liftIO $ fromJSValUncheckedListOf path_arr
      modify'
        ( \s ->
            s
              { _hdlstateSyncState = SyncState [path],
                _hdlstateOverlayUpdated = True
              }
        )
      let hsh = hash path
          msg = NewStroke (hsh, path)
      liftIO $ WS.send (JSS.pack . T.unpack . serialize $ msg) sock
    _ -> drawingMode xys

erasingMode :: [CommitId] -> (Double, Double) -> MainCoroutine ()
erasingMode hitted0 (x0, y0) = do
  ev <- nextevent
  case ev of
    PointerMove (cx, cy) -> do
      HoodleState svg _ _ _ (DocState _ strks) _ _ <- get
      (x, y) <- liftIO $ J.getXYinSVG svg (cx, cy)
      let !hitted = map fst $ filter (doesLineHitStrk ((x0, y0), (x, y)) . snd) strks
          !hitted' = nub $ sort (hitted ++ hitted0)
      liftIO $ traverse_ (J.strokeChangeColor svg . ("stroke" ++) . show . unCommitId) hitted
      erasingMode hitted' (x, y)
    PointerUp _ -> do
      HoodleState svg _ _ sock _ _ _ <- get
      when (not . null $ hitted0) $ liftIO $ do
        let msg = DeleteStrokes hitted0
        WS.send (JSS.pack . T.unpack . serialize $ msg) sock
    _ -> erasingMode hitted0 (x0, y0)

initmc :: MainObj ()
initmc = ReaderT $ (\(Arg DoEvent ev) -> guiProcess ev)

main :: IO ()
main = do
  evar <- newEmptyMVar :: IO EventVar
  xstate <- setupCallback evar
  putMVar evar . Just $ D.driver simplelogger (world xstate initmc)
