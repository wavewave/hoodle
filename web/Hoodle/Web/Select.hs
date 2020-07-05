{-# OPTIONS_GHC -Werror -Wall #-}

module Hoodle.Web.Select where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState (get, put))
import Data.Foldable (toList, traverse_)
import Data.Sequence (Seq, ViewR (..), singleton, viewr, (|>))
import qualified Data.Sequence as Seq (fromList, length)
import Hoodle.Web.Default (nextevent)
import qualified Hoodle.Web.ForeignJS as J
import Hoodle.Web.Type.Coroutine (MainCoroutine)
import Hoodle.Web.Type.Event (UserEvent (..))
import Hoodle.Web.Type.State (DocState (..), HoodleState (..))
import Hoodle.Web.Util
  ( enclosedStrokes,
    transformPathFromCanvasToSVG,
  )
import Message (CommitId (..))

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
