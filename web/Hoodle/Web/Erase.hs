{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Werror -Wall #-}

module Hoodle.Web.Erase where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get))
import Coroutine (MainCoroutine)
import Data.Foldable (toList, traverse_)
import qualified Data.JSString as JSS (pack)
import Data.List (nub, sort)
import Data.Sequence (Seq, ViewR (..), singleton, viewr, (|>))
import qualified Data.Sequence as Seq (length)
import qualified Data.Text as T
import qualified ForeignJS as J
import Hoodle.Web.Default (nextevent)
import Hoodle.Web.Type.Event (UserEvent (..))
import Hoodle.Web.Util
  ( intersectingStrokes,
    transformPathFromCanvasToSVG,
  )
import qualified JavaScript.Web.WebSocket as WS
import Message
  ( C2SMsg (DeleteStrokes),
    CommitId (..),
    TextSerializable (serialize),
  )
import State (DocState (..), HoodleState (..))

eraseUpdatePeriod :: Int
eraseUpdatePeriod = 10

erasingMode :: [CommitId] -> Seq (Double, Double) -> MainCoroutine ()
erasingMode hstrks0 cxys = do
  ev <- nextevent
  case ev of
    PointerMove cxy -> do
      HoodleState svg _ _ _ (DocState _ strks) _ _ <- get
      case viewr cxys of
        _ :> _ ->
          if Seq.length cxys >= eraseUpdatePeriod
            then do
              xys <- liftIO $ transformPathFromCanvasToSVG svg (toList cxys)
              let hstrks = intersectingStrokes xys strks
              liftIO $
                traverse_ (J.strokeChangeColor svg . ("stroke" ++) . show . unCommitId) hstrks
              let !hstrks' = nub $ sort (hstrks ++ hstrks0)
              erasingMode hstrks' (singleton cxy)
            else erasingMode hstrks0 (cxys |> cxy)
        _ -> pure ()
    PointerUp _ -> do
      HoodleState _ _ _ sock _ _ _ <- get
      when (not . null $ hstrks0) $ liftIO $ do
        let msg = DeleteStrokes hstrks0
        WS.send (JSS.pack . T.unpack . serialize $ msg) sock
    _ -> erasingMode hstrks0 cxys
