{-# LANGUAGE BangPatterns #-}

module Hoodle.Web.Erase where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get))
import Data.Foldable (toList, traverse_)
import Data.List (nub, sort)
import Data.Sequence (Seq, ViewR (..), singleton, viewr, (|>))
import qualified Data.Sequence as Seq (length)
import Hoodle.Web.Default (nextevent)
import qualified Hoodle.Web.ForeignJS as J
import Hoodle.Web.Type.Coroutine (MainCoroutine)
import Hoodle.Web.Type.Event (UserEvent (..))
import Hoodle.Web.Type.State
  ( docstateData,
    hdlstateDocState,
    hdlstateSVGBox,
    hdlstateWebSocket,
  )
import Hoodle.Web.Util
  ( intersectingStrokes,
    sendBinary,
    transformPathFromCanvasToSVG,
  )
import Lens.Micro ((<&>), (^.))
import Message
  ( C2SMsg (DeleteStrokes),
    CommitId (..),
    -- TextSerializable (serialize),
  )

eraseUpdatePeriod :: Int
eraseUpdatePeriod = 10

erasingMode :: [CommitId] -> Seq (Double, Double) -> MainCoroutine ()
erasingMode hstrks0 cxys = do
  ev <- nextevent
  case ev of
    PointerMove cxy -> do
      s <- get
      let svg = s ^. hdlstateSVGBox
          strks = s ^. hdlstateDocState . docstateData
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
      sock <- get <&> (^. hdlstateWebSocket)
      when (not . null $ hstrks0) $
        liftIO $ do
          let msg = DeleteStrokes hstrks0
          sendBinary sock msg
    _ -> erasingMode hstrks0 cxys
