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
import qualified Data.Sequence as Seq (length)
import qualified Data.Text as T
import Event (AllEvent (..), SystemEvent (..), UserEvent (..))
import qualified ForeignJS as J
import GHCJS.Marshal (FromJSVal (..), ToJSVal (..))
import GHCJS.Types (JSVal)
import Handler (setupCallback)
import Hoodle.HitTest (do2BBoxIntersect, doesLineHitStrk)
import Hoodle.HitTest.Type (BBox (..), BBoxed (..), GetBBoxable (getBBox))
import qualified JavaScript.Web.WebSocket as WS
import Message
  ( C2SMsg (DeleteStrokes, NewStroke, SyncRequest),
    Commit (Add, Delete),
    CommitId (..),
    TextSerializable (serialize),
    commitId,
  )
import State (DocState (..), HoodleState (..), RStroke (..), SyncState (..))

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
          f !acc (Add i xys) = acc ++ [BBoxed (RStroke i xys) (pathBBox xys)]
          f !acc (Delete i js) = filter (\(BBoxed (RStroke j _) _) -> not (j `elem` js)) acc
  put $
    st
      { _hdlstateDocState = DocState i dat',
        _hdlstateOverlayUpdated = True
      }
sysevent ERefresh = do
  s@(HoodleState _ cvs offcvs _ _ _ isUpdated) <- get
  when isUpdated $ do
    liftIO $ J.js_refresh cvs offcvs
    put $ s {_hdlstateOverlayUpdated = False}

guiProcess :: AllEvent -> MainCoroutine ()
guiProcess (SysEv sev) = sysevent sev >> nextevent >>= penReady
guiProcess (UsrEv uev) = penReady uev

penReady :: UserEvent -> MainCoroutine ()
penReady ev = do
  case ev of
    PointerDown (cx, cy) ->
      drawingMode (singleton (cx, cy))
    ToPenMode -> pure ()
    ToEraserMode -> nextevent >>= eraserReady
    _ -> pure ()
  nextevent >>= penReady

eraserReady :: UserEvent -> MainCoroutine ()
eraserReady ev = do
  case ev of
    ToPenMode -> nextevent >>= penReady
    PointerDown (cx0, cy0) -> do
      HoodleState svg _ _ _ _ _ _ <- get
      erasingMode [] (singleton (cx0, cy0))
    _ -> pure ()
  nextevent >>= eraserReady

drawingMode :: Seq (Double, Double) -> MainCoroutine ()
drawingMode cxys = do
  ev <- nextevent
  case ev of
    PointerMove cxy@(cx, cy) -> do
      s@(HoodleState _ cvs offcvs _ _ _ _) <- get
      case viewr cxys of
        _ :> (cx0, cy0) -> liftIO $ J.js_overlay_point cvs offcvs cx0 cy0 cx cy
        _ -> pure ()
      put $ s {_hdlstateOverlayUpdated = True}
      drawingMode (cxys |> cxy)
    PointerUp cxy -> do
      HoodleState svg _ _ sock _ _ _ <- get
      let cxys' = cxys |> cxy
      path_arr <-
        liftIO $
          J.js_to_svg_point_array svg =<< toJSValListOf (toList cxys')
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
    _ -> drawingMode cxys

pathBBox :: [(Double, Double)] -> BBox
pathBBox path =
  let xs = map fst path
      ys = map snd path
   in BBox
        { bbox_upperleft = (minimum xs, minimum ys),
          bbox_lowerright = (maximum xs, maximum ys)
        }

findHitStrokes ::
  JSVal ->
  Seq (Double, Double) ->
  [BBoxed RStroke] ->
  IO [CommitId]
findHitStrokes svg cxys strks = do
  xys_arr <-
    J.js_to_svg_point_array svg =<< toJSValListOf (toList cxys)
  xys <- fromJSValUncheckedListOf xys_arr
  let bbox1 = pathBBox xys
  let pairs = zip xys (tail xys)
      hitstrks = flip concatMap pairs $ \((x0, y0), (x, y)) ->
        map rstrokeCommitId
          $ filter (doesLineHitStrk ((x0, y0), (x, y)) . rstrokePath)
          $ map bbxed_content
          $ filter (do2BBoxIntersect bbox1 . getBBox)
          $ strks
  pure hitstrks

updateEraseStatePeriod :: Int
updateEraseStatePeriod = 10

erasingMode :: [CommitId] -> Seq (Double, Double) -> MainCoroutine ()
erasingMode hstrks0 cxys = do
  ev <- nextevent
  case ev of
    PointerMove cxy@(cx, cy) -> do
      s@(HoodleState svg cvs offcvs _ (DocState _ strks) _ _) <- get
      case viewr cxys of
        _ :> (cx0, cy0) -> do
          liftIO $ J.js_overlay_point cvs offcvs cx0 cy0 cx cy
          put $ s {_hdlstateOverlayUpdated = True}
          if Seq.length cxys >= updateEraseStatePeriod
            then do
              hstrks <- liftIO $ findHitStrokes svg cxys strks
              liftIO $
                traverse_ (J.strokeChangeColor svg . ("stroke" ++) . show . unCommitId) hstrks
              let !hstrks' = nub $ sort (hstrks ++ hstrks0)
              erasingMode hstrks' (singleton cxy)
            else erasingMode hstrks0 (cxys |> cxy)
        _ -> pure ()
    PointerUp _ -> do
      HoodleState svg _ _ sock _ _ _ <- get
      when (not . null $ hstrks0) $ liftIO $ do
        let msg = DeleteStrokes hstrks0
        WS.send (JSS.pack . T.unpack . serialize $ msg) sock
    _ -> erasingMode hstrks0 cxys

initmc :: MainObj ()
initmc = ReaderT $ (\(Arg DoEvent ev) -> guiProcess ev)

main :: IO ()
main = do
  evar <- newEmptyMVar :: IO EventVar
  xstate <- setupCallback evar
  putMVar evar . Just $ D.driver simplelogger (world xstate initmc)
