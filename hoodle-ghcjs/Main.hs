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
import qualified Control.Monad.Trans.Crtn.Driver as D (driver)
import Control.Monad.Trans.Crtn.Object (Arg (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Coroutine
  ( EventVar,
    MainCoroutine,
    MainObj,
    MainOp (DoEvent),
    nextevent,
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
import Event (AllEvent (..))
import qualified ForeignJS as J
import GHCJS.Marshal (FromJSVal (..), ToJSVal (..))
import GHCJS.Types (JSVal)
import Handler (setupCallback)
import HitTest (doesLineHitStrk)
import qualified JavaScript.Web.WebSocket as WS
import Message
  ( C2SMsg (DeleteStrokes, NewStroke, SyncRequest),
    Commit (Add, Delete),
    TextSerializable (serialize),
    commitId,
  )
import State (DocState (..), HoodleState (..), SyncState (..))

guiProcess :: AllEvent -> MainCoroutine ()
guiProcess = penReady

penReady :: AllEvent -> MainCoroutine ()
penReady ev = do
  case ev of
    ERegisterStroke (s', _hsh') -> do
      HoodleState _ _ _ sock (DocState n _) _ <- get
      when (s' > n) $ liftIO $ do
        let msg = SyncRequest (n, s')
        WS.send (JSS.pack . T.unpack . serialize $ msg) sock
    -- TODO: refactor this out.
    EDataStrokes commits -> do
      liftIO $ putStrLnAndFlush $ show commits -- show (map commitId commits)
      st@(HoodleState svg _ offcvs _ (DocState _ dat0) _) <- get
      liftIO $ do
        J.js_clear_overlay offcvs
        traverse_
          ( \case
              Add i xys -> J.drawPath svg ("stroke" ++ show i) xys
              Delete _ js -> do
                putStrLnAndFlush
                  ("in pen: " ++ show js)
                traverse_
                  (J.strokeRemove svg . ("stroke" ++) . show)
                  js
          )
          commits
      let i = maximum (map commitId commits)
          dat' = foldl' f dat0 commits
            where
              f !acc (Add i xys) = acc ++ [(i, xys)]
              f !acc (Delete i js) = filter (\(j, _) -> not (j `elem` js)) acc
      put $ st {_hdlstateDocState = DocState i dat'}
    PointerDown (x, y) ->
      drawingMode (singleton (x, y))
    ToPenMode -> pure ()
    ToEraserMode -> nextevent >>= eraserReady
    _ -> pure ()
  nextevent >>= penReady

getXYinSVG :: JSVal -> (Double, Double) -> IO (Double, Double)
getXYinSVG svg (x0, y0) = do
  r <- J.js_to_svg_point svg x0 y0
  [x, y] <- fromJSValUncheckedListOf r
  pure (x, y)

eraserReady :: AllEvent -> MainCoroutine ()
eraserReady ev = do
  case ev of
    ToPenMode -> nextevent >>= penReady
    PointerDown (x0, y0) -> do
      HoodleState svg _ _ _ _ _ <- get
      (x, y) <- liftIO $ getXYinSVG svg (x0, y0)
      erasingMode [] (x, y)
    -- TODO: need to refactor this out. using SysEv.
    ERegisterStroke (s', _hsh') -> do
      HoodleState _ _ _ sock (DocState n _) _ <- get
      when (s' > n) $ liftIO $ do
        let msg = SyncRequest (n, s')
        WS.send (JSS.pack . T.unpack . serialize $ msg) sock
    -- TODO: refactor this out.
    EDataStrokes commits -> do
      st@(HoodleState svg _ offcvs _ (DocState _ dat0) _) <- get
      liftIO $ do
        J.js_clear_overlay offcvs
        traverse_
          ( \case
              Add i xys -> J.drawPath svg ("stroke" ++ show i) xys
              Delete _ js -> do
                putStrLnAndFlush ("in erase: " ++ show js)
                traverse_
                  (J.strokeRemove svg . ("stroke" ++) . show)
                  js
          )
          commits
      let i = maximum (map commitId commits)
          dat' = foldl' f dat0 commits
            where
              f !acc (Add i xys) = acc ++ [(i, xys)]
              f !acc (Delete i js) = filter (\(j, _) -> not (j `elem` js)) acc
      put $ st {_hdlstateDocState = DocState i dat'}
    _ -> pure ()
  nextevent >>= eraserReady

drawingMode :: Seq (Double, Double) -> MainCoroutine ()
drawingMode xys = do
  ev <- nextevent
  case ev of
    PointerMove xy@(x, y) -> do
      HoodleState _svg cvs offcvs _ _ _ <- get
      case viewr xys of
        _ :> (x0, y0) -> liftIO $ J.js_overlay_point cvs offcvs x0 y0 x y
        _ -> pure ()
      drawingMode (xys |> xy)
    PointerUp xy -> do
      HoodleState svg _ _ sock _ _ <- get
      let xys' = xys |> xy
      path_arr <-
        liftIO $
          J.js_to_svg_point_array svg =<< toJSValListOf (toList xys')
      path <- liftIO $ fromJSValUncheckedListOf path_arr
      modify' (\s -> s {_hdlstateSyncState = SyncState [path]})
      let hsh = hash path
          msg = NewStroke (hsh, path)
      liftIO $ WS.send (JSS.pack . T.unpack . serialize $ msg) sock
    _ -> drawingMode xys

erasingMode :: [Int] -> (Double, Double) -> MainCoroutine ()
erasingMode hitted0 (x0, y0) = do
  ev <- nextevent
  case ev of
    PointerMove (cx, cy) -> do
      HoodleState svg _ _ _ (DocState _ strks) _ <- get
      (x, y) <- liftIO $ getXYinSVG svg (cx, cy)
      let !hitted = map fst $ filter (doesLineHitStrk ((x0, y0), (x, y)) . snd) strks
          !hitted' = nub $ sort (hitted ++ hitted0)
      liftIO $ traverse_ (J.strokeChangeColor svg . ("stroke" ++) . show) hitted
      erasingMode hitted' (x, y)
    PointerUp _ -> do
      HoodleState svg _ _ sock _ _ <- get
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
