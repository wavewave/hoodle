{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get, put))
import qualified Control.Monad.Trans.Crtn.Driver as D (driver)
import Control.Monad.Trans.Crtn.Object (Arg (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Coroutine
  ( EventVar,
    MainCoroutine,
    MainObj,
    MainOp (DoEvent),
    simplelogger,
    world,
  )
import Data.Foldable (toList, traverse_)
import Data.Sequence (Seq, ViewR (..), empty, singleton, viewr, (|>))
import qualified Data.Sequence as Seq (fromList, length)
import Event (AllEvent (..), UserEvent (..))
import qualified ForeignJS as J
import Handler (setupCallback)
import Hoodle.Web.Default (nextevent, sysevent)
import Hoodle.Web.Erase (erasingMode)
import Hoodle.Web.Pen (drawingMode)
import Hoodle.Web.Util
  ( enclosedStrokes,
    transformPathFromCanvasToSVG,
  )
import Message (CommitId (..))
import State (DocState (..), HoodleState (..))

guiProcess :: AllEvent -> MainCoroutine ()
guiProcess (SysEv sev) = sysevent sev >> nextevent >>= penReady
guiProcess (UsrEv uev) = penReady uev

penReady :: UserEvent -> MainCoroutine ()
penReady ev = do
  case ev of
    PointerDown (cx, cy) ->
      drawingMode (singleton (cx, cy))
    ToEraserMode -> nextevent >>= eraserReady
    ToSelectMode -> nextevent >>= selectReady
    _ -> pure ()
  nextevent >>= penReady

eraserReady :: UserEvent -> MainCoroutine ()
eraserReady ev = do
  case ev of
    ToPenMode -> nextevent >>= penReady
    ToSelectMode -> nextevent >>= selectReady
    PointerDown (cx0, cy0) -> do
      HoodleState _ _ _ _ _ _ _ <- get
      erasingMode [] (singleton (cx0, cy0))
    _ -> pure ()
  nextevent >>= eraserReady

selectReady :: UserEvent -> MainCoroutine ()
selectReady ev = do
  case ev of
    ToPenMode -> nextevent >>= penReady
    ToEraserMode -> nextevent >>= eraserReady
    PointerDown (cx0, cy0) -> do
      HoodleState _ _ _ _ _ _ _ <- get
      lassoMode empty (singleton (cx0, cy0))
    _ -> pure ()
  nextevent >>= selectReady

lassoUpdatePeriod :: Int
lassoUpdatePeriod = 10

lassoMode :: Seq (Double, Double) -> Seq (Double, Double) -> MainCoroutine ()
lassoMode lasso cxys = do
  ev <- nextevent
  case ev of
    PointerMove cxy@(cx, cy) -> do
      s@(HoodleState svg cvs offcvs _ (DocState _ strks) _ _) <- get
      case viewr cxys of
        _ :> (cx0, cy0) ->
          if Seq.length cxys >= lassoUpdatePeriod
            then do
              liftIO $ J.js_overlay_point cvs offcvs cx0 cy0 cx cy
              put $ s {_hdlstateOverlayUpdated = True}
              dLasso <- liftIO $ transformPathFromCanvasToSVG svg (toList cxys)
              let lasso' = lasso <> Seq.fromList dLasso
                  hstrks = enclosedStrokes lasso' strks
              liftIO $
                traverse_ (J.strokeChangeColor svg . ("stroke" ++) . show . unCommitId) hstrks
              lassoMode lasso' (singleton cxy)
            else lassoMode lasso (cxys |> cxy)
        _ -> pure () -- this should not happen.
    PointerUp _ -> pure ()
    _ -> lassoMode lasso cxys

initmc :: MainObj ()
initmc = ReaderT $ (\(Arg DoEvent ev) -> guiProcess ev)

main :: IO ()
main = do
  evar <- newEmptyMVar :: IO EventVar
  xstate <- setupCallback evar
  putMVar evar . Just $ D.driver simplelogger (world xstate initmc)
