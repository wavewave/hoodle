{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Monad.State (MonadState (get))
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
import Data.Sequence (empty, singleton)
import Handler (setupCallback)
import Hoodle.Web.Default (nextevent, sysevent)
import Hoodle.Web.Erase (erasingMode)
import Hoodle.Web.Pen (drawingMode)
import Hoodle.Web.Select (lassoMode)
import Hoodle.Web.Type.Event (AllEvent (..), UserEvent (..))
import State (HoodleState (..))

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

initmc :: MainObj ()
initmc = ReaderT $ (\(Arg DoEvent ev) -> guiProcess ev)

main :: IO ()
main = do
  evar <- newEmptyMVar :: IO EventVar
  xstate <- setupCallback evar
  putMVar evar . Just $ D.driver simplelogger (world xstate initmc)
