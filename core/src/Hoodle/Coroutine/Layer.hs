{-# LANGUAGE ScopedTypeVariables #-}

module Hoodle.Coroutine.Layer where

import Control.Category (id, (.))
import Control.Lens (set, view)
import Control.Monad.State (get, liftIO, modify)
import Data.Hoodle.Generic (glayers)
import Data.Hoodle.Zipper
  ( appendGoLast,
    currIndex,
    deleteCurrent,
    lengthSZ,
    moveLeft,
    moveRight,
    moveTo,
  )
import Data.IORef (newIORef, readIORef)
import qualified Data.IntMap as M
import Graphics.Hoodle.Render.Type
  ( emptyRLayer,
    hPage2RPage,
    issueSurfaceID,
  )
import qualified Graphics.UI.Gtk as Gtk
import Hoodle.Accessor (getCurrentPageEitherFromHoodleModeState)
import Hoodle.Coroutine.Commit (commit)
import Hoodle.Coroutine.Draw (invalidateAll)
import Hoodle.ModelAction.Layer (layerChooseDialog)
import Hoodle.ModelAction.Page (getPageMap, setPageMap, updatePageAll)
import Hoodle.Type.Alias (EditMode, Page)
import Hoodle.Type.Canvas
  ( currentPageNum,
    forBoth',
    unboxBiAct,
  )
import Hoodle.Type.Coroutine (MainCoroutine)
import Hoodle.Type.HoodleState
  ( HoodleModeState,
    UnitHoodle,
    currentCanvasInfo,
    currentUnit,
    hoodleModeState,
    unitHoodles,
  )
--
import Prelude hiding (id, (.))

layerAction ::
  (HoodleModeState -> Int -> Page EditMode -> MainCoroutine HoodleModeState) ->
  MainCoroutine UnitHoodle
layerAction action = do
  xst <- get
  forBoth' unboxBiAct (fsingle xst) . view (unitHoodles . currentUnit . currentCanvasInfo) $ xst
  where
    fsingle xstate cvsInfo = do
      let uhdl = view (unitHoodles . currentUnit) xstate
          epage = getCurrentPageEitherFromHoodleModeState cvsInfo hdlmodst
          cpn = view currentPageNum cvsInfo
          hdlmodst = view hoodleModeState uhdl
      newhdlmodst <- either (action hdlmodst cpn) (action hdlmodst cpn . hPage2RPage) epage
      liftIO (updatePageAll newhdlmodst . set hoodleModeState newhdlmodst $ uhdl)

-- |
makeNewLayer :: MainCoroutine ()
makeNewLayer = do
  xst <- get
  commit . flip (set (unitHoodles . currentUnit)) xst =<< layerAction newlayeraction
  invalidateAll
  where
    newlayeraction hdlmodst cpn page = do
      sfcid <- issueSurfaceID
      let lyrzipper = view glayers page
          emptylyr = emptyRLayer sfcid
          nlyrzipper = appendGoLast lyrzipper emptylyr
          npage = set glayers nlyrzipper page
      return . setPageMap (M.adjust (const npage) cpn . getPageMap $ hdlmodst) $ hdlmodst

gotoNextLayer :: MainCoroutine ()
gotoNextLayer = do
  modify . set (unitHoodles . currentUnit) =<< layerAction nextlayeraction
  invalidateAll
  where
    nextlayeraction hdlmodst cpn page = do
      let lyrzipper = view glayers page
          mlyrzipper = moveRight lyrzipper
          npage = maybe page (\x -> set glayers x page) mlyrzipper
      return . setPageMap (M.adjust (const npage) cpn . getPageMap $ hdlmodst) $ hdlmodst

gotoPrevLayer :: MainCoroutine ()
gotoPrevLayer = do
  modify . set (unitHoodles . currentUnit) =<< layerAction prevlayeraction
  invalidateAll
  where
    prevlayeraction hdlmodst cpn page = do
      let lyrzipper = view glayers page
          mlyrzipper = moveLeft lyrzipper
          npage = maybe page (\x -> set glayers x page) mlyrzipper
      return . setPageMap (M.adjust (const npage) cpn . getPageMap $ hdlmodst) $ hdlmodst

gotoLayerAt :: Int -> MainCoroutine ()
gotoLayerAt n = do
  modify . set (unitHoodles . currentUnit) =<< layerAction gotoaction
  invalidateAll
  where
    gotoaction hdlmodst cpn page = do
      let lyrzipper = view glayers page
          mlyrzipper = moveTo n lyrzipper
          npage = maybe page (\x -> set glayers x page) mlyrzipper
      return . setPageMap (M.adjust (const npage) cpn . getPageMap $ hdlmodst) $ hdlmodst

deleteCurrentLayer :: MainCoroutine ()
deleteCurrentLayer = do
  xst <- get
  commit . flip (set (unitHoodles . currentUnit)) xst =<< layerAction deletelayeraction
  invalidateAll
  where
    deletelayeraction hdlmodst cpn page = do
      let lyrzipper = view glayers page
          mlyrzipper = deleteCurrent lyrzipper
          npage = maybe page (\x -> set glayers x page) mlyrzipper
      return . setPageMap (M.adjust (const npage) cpn . getPageMap $ hdlmodst) $ hdlmodst

startGotoLayerAt :: MainCoroutine ()
startGotoLayerAt =
  forBoth' unboxBiAct fsingle . view (unitHoodles . currentUnit . currentCanvasInfo) =<< get
  where
    fsingle cvsInfo = do
      xstate <- get
      let uhdl = view (unitHoodles . currentUnit) xstate
          hdlmodst = view hoodleModeState uhdl
          epage = getCurrentPageEitherFromHoodleModeState cvsInfo hdlmodst
          page = either id hPage2RPage epage
          lyrzipper = view glayers page
          cidx = currIndex lyrzipper
          len = lengthSZ lyrzipper
      lref <- liftIO $ newIORef cidx
      dialog <- liftIO (layerChooseDialog lref cidx len)
      res <- liftIO $ Gtk.dialogRun dialog
      case res of
        Gtk.ResponseDeleteEvent -> liftIO $ Gtk.widgetDestroy dialog
        Gtk.ResponseOk -> do
          liftIO $ Gtk.widgetDestroy dialog
          newnum <- liftIO (readIORef lref)
          gotoLayerAt newnum
        Gtk.ResponseCancel -> liftIO $ Gtk.widgetDestroy dialog
        _ -> error "??? in fileOpen "
      return ()
