{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Hoodle.GUI.Reflect
-- Copyright   : (c) 2013, 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
module Hoodle.GUI.Reflect where

import Control.Lens (Lens', view, (.~), (^.), _1, _2, _3)
import Control.Monad (when)
import qualified Control.Monad.State as St
import Control.Monad.Trans (liftIO)
import Data.Array.MArray (writeArray)
import qualified Data.Foldable as F (forM_, mapM_)
import qualified Data.Map as M (lookup)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import qualified Graphics.UI.Gtk as Gtk
import Hoodle.Accessor (pureUpdateUhdl)
import Hoodle.Coroutine.Draw
  ( doIOaction_,
    invalidateAll,
  )
import Hoodle.GUI.Menu
  ( color2Int,
    newPageMode2Int,
    penType2Int,
    point2Int,
  )
import Hoodle.Type.Canvas
  ( CanvasId,
    currEraser,
    currHighlighter,
    currPen,
    currentPageNum,
    drawArea,
    forBoth',
    pageArrangement,
    penColor,
    penSet,
    penType,
    penWidth,
    unboxBiAct,
    viewInfo,
  )
import Hoodle.Type.Coroutine (MainCoroutine)
import Hoodle.Type.Enum
  ( PenColor (ColorRGBA),
    PenType (..),
    penColorRGBAmap,
    selectType,
  )
import Hoodle.Type.Event (AllEvent (UsrEv), UserEvent (ActionOrdered))
import Hoodle.Type.HoodleState
  ( HoodleState,
    UIComponentSignalHandler,
    UnitHoodle,
    currentCanvasInfo,
    currentUnit,
    cursorInfo,
    doesUseVariableCursor,
    getCurrentCanvasId,
    gtkUIManager,
    hoodleModeState,
    hoodleModeStateEither,
    newPageMode,
    newPageModeSignal,
    pageModeSignal,
    penColorSignal,
    penInfo,
    penModeSignal,
    penPointSignal,
    selectInfo,
    setCurrentCanvasId,
    settings,
    uiComponentSignalHandler,
    unitHoodles,
  )
import Hoodle.Type.PageArrangement
  ( CanvasCoordinate (..),
    PageCoordinate (..),
    PageNum (..),
  )
import Hoodle.Type.Predefined
  ( maxCursorHeight,
    maxCursorWidth,
  )
import Hoodle.Util (msgShout, (#))
import Hoodle.View.Coordinate
  ( desktop2Canvas,
    makeCanvasGeometry,
    page2Desktop,
  )

-- |
changeCurrentCanvasId :: CanvasId -> MainCoroutine UnitHoodle
changeCurrentCanvasId cid = do
  xst <- St.get
  let uhdl = view (unitHoodles . currentUnit) xst
  case setCurrentCanvasId cid uhdl of
    Nothing -> return uhdl
    Just uhdl' -> do
      pureUpdateUhdl (const uhdl')
      reflectViewModeUI
      return uhdl'

-- | check current canvas id and new active canvas id and invalidate if it's
--   changed.
chkCvsIdNInvalidate :: CanvasId -> MainCoroutine ()
chkCvsIdNInvalidate cid = do
  currcid <- St.gets (getCurrentCanvasId . view (unitHoodles . currentUnit))
  when (currcid /= cid) (changeCurrentCanvasId cid >> invalidateAll)

-- | block signal for act
blockWhile :: (Gtk.GObjectClass w) => Maybe (Gtk.ConnectId w) -> IO () -> IO ()
blockWhile msig act = do
  -- F.mapM_ (\_ -> print "signal will be blocked") msig
  F.mapM_ Gtk.signalBlock msig >> act >> F.mapM_ Gtk.signalUnblock msig

-- | reflect view mode UI for current canvas info
reflectViewModeUI :: MainCoroutine ()
reflectViewModeUI = do
  xstate <- St.get
  let uhdl = view (unitHoodles . currentUnit) xstate
      cinfobox = view currentCanvasInfo uhdl
      ui = view gtkUIManager xstate
  let mconnid = view (uiComponentSignalHandler . pageModeSignal) xstate
  agr <- liftIO $ Gtk.uiManagerGetActionGroups ui
  ra1 <-
    maybe (error "reflectUI") return
      =<< liftIO (Gtk.actionGroupGetAction (head agr) "ONEPAGEA")
  let wra1 = Gtk.castToRadioAction ra1
  unboxBiAct (pgmodupdate_s mconnid wra1) (pgmodupdate_c mconnid wra1) cinfobox
  return ()
  where
    pgmodupdate_s mconnid wra1 _cinfo = do
      liftIO $
        blockWhile mconnid $
          Gtk.set wra1 [Gtk.radioActionCurrentValue Gtk.:= 1]
    pgmodupdate_c mconnid wra1 _cinfo = do
      liftIO $
        blockWhile mconnid $
          Gtk.set wra1 [Gtk.radioActionCurrentValue Gtk.:= 0]

-- |
reflectPenModeUI :: MainCoroutine ()
reflectPenModeUI = do
  reflectUIRadio penModeSignal "PENA" f
  reflectCursor False
  where
    f xst =
      Just $
        hoodleModeStateEither ((view hoodleModeState . view (unitHoodles . currentUnit)) xst)
          # either
            (\_ -> (penType2Int . Left . view (penInfo . penType)) xst)
            (\_ -> (penType2Int . Right . view (selectInfo . selectType)) xst)

-- |
reflectPenColorUI :: MainCoroutine ()
reflectPenColorUI = do
  reflectUIRadio penColorSignal "BLUEA" f
  reflectCursor False
  where
    f xst =
      let mcolor =
            case view (penInfo . penType) xst of
              PenWork -> Just (view (penInfo . penSet . currPen . penColor) xst)
              HighlighterWork -> Just (view (penInfo . penSet . currHighlighter . penColor) xst)
              _ -> Nothing
       in fmap color2Int mcolor

-- |
reflectPenWidthUI :: MainCoroutine ()
reflectPenWidthUI = do
  reflectUIRadio penPointSignal "PENVERYFINEA" f
  reflectCursor False
  where
    f xst =
      case view (penInfo . penType) xst of
        PenWork ->
          ( Just . point2Int PenWork
              . view (penInfo . penSet . currPen . penWidth)
          )
            xst
        HighlighterWork ->
          let x =
                ( Just . point2Int HighlighterWork
                    . view (penInfo . penSet . currHighlighter . penWidth)
                )
                  xst
           in -- y = view (penInfo.penSet.currHighlighter.penWidth) xst
              x
        EraserWork ->
          ( Just . point2Int EraserWork
              . view (penInfo . penSet . currEraser . penWidth)
          )
            xst
        _ -> Nothing

-- |
reflectNewPageModeUI :: MainCoroutine ()
reflectNewPageModeUI =
  reflectUIRadio newPageModeSignal "NEWPAGEPLAINA" (Just . newPageMode2Int . (^. settings . newPageMode))

-- |
reflectUIRadio ::
  Lens' UIComponentSignalHandler (Maybe (Gtk.ConnectId Gtk.RadioAction)) ->
  String ->
  (HoodleState -> Maybe Int) ->
  MainCoroutine ()
reflectUIRadio lnz name f = do
  xst <- St.get
  let ui = view gtkUIManager xst
      mconnid = view (uiComponentSignalHandler . lnz) xst
  agr <- liftIO $ Gtk.uiManagerGetActionGroups ui
  Just pma <- liftIO $ Gtk.actionGroupGetAction (head agr) name
  let wpma = Gtk.castToRadioAction pma
  update xst wpma mconnid
  where
    update xst wpma mconnid = do
      f xst
        # maybe
          (return ())
          ( \v ->
              doIOaction_ $ blockWhile mconnid (Gtk.set wpma [Gtk.radioActionCurrentValue Gtk.:= v])
          )

-- | this function must be moved to GUI.Reflect
reflectUIToggle :: Gtk.UIManager -> String -> Bool -> IO ()
reflectUIToggle ui str b = do
  agr <-
    Gtk.uiManagerGetActionGroups ui >>= \case
      [] -> error "No action group?"
      y : _ -> return y
  Just savea <- Gtk.actionGroupGetAction agr str -- ("SAVEA" :: String)
  Gtk.actionSetSensitive savea b

-- |
reflectCursor :: Bool -> MainCoroutine ()
reflectCursor isforced = do
  xst <- St.get
  let b = view (settings . doesUseVariableCursor) xst
      pinfo = view penInfo xst
      pcolor = view (penSet . currPen . penColor) pinfo
      pwidth = view (penSet . currPen . penWidth) pinfo
      cinfo = view cursorInfo xst
      (ccolor, cwidth, cvar) = cinfo
  when (pcolor /= ccolor || pwidth /= cwidth || b /= cvar || isforced) $ do
    msgShout "reflectCursor: change cursor"
    St.put . (cursorInfo . _1 .~ pcolor) . (cursorInfo . _2 .~ pwidth) . (cursorInfo . _3 .~ b) $ xst
    doIOaction_ $
      if b
        then varyCursor xst
        else do
          let uhdl = view (unitHoodles . currentUnit) xst
              cinfobox = view currentCanvasInfo uhdl
              canvas = forBoth' unboxBiAct (view drawArea) cinfobox
          Just win <- Gtk.widgetGetWindow canvas
          Gtk.postGUIAsync (Gtk.drawWindowSetCursor win Nothing)
          return (UsrEv ActionOrdered)
  where
    varyCursor xst = do
      putStrLn "reflectCursor : inside act"

      -- Gtk.postGUIAsync $ do
      let uhdl = view (unitHoodles . currentUnit) xst
          -- mcur       = view cursorInfo xst
          cinfobox = view currentCanvasInfo uhdl
          canvas = forBoth' unboxBiAct (view drawArea) cinfobox
          cpn =
            PageNum $
              forBoth' unboxBiAct (view currentPageNum) cinfobox
          pinfo = view penInfo xst
          pcolor = view (penSet . currPen . penColor) pinfo
          pwidth = view (penSet . currPen . penWidth) pinfo
      Just win <- Gtk.widgetGetWindow canvas
      dpy <- Gtk.widgetGetDisplay canvas

      geometry <-
        forBoth'
          unboxBiAct
          ( \c ->
              let arr = view (viewInfo . pageArrangement) c
               in makeCanvasGeometry cpn arr canvas
          )
          cinfobox
      let p2c = desktop2Canvas geometry . page2Desktop geometry
          CvsCoord (x0, _y0) = p2c (cpn, PageCoord (0, 0))
          CvsCoord (x1, _y1) = p2c (cpn, PageCoord (pwidth, pwidth))
          cursize = x1 - x0
          (r, g, b, a) = case pcolor of
            ColorRGBA r' g' b' a' -> (r', g', b', a')
            _ -> fromMaybe (0, 0, 0, 1) (M.lookup pcolor penColorRGBAmap)
      pb <- Gtk.pixbufNew Gtk.ColorspaceRgb True 8 maxCursorWidth maxCursorHeight
      let numPixels = maxCursorWidth * maxCursorHeight
      pbData <- (Gtk.pixbufGetPixels pb :: IO (Gtk.PixbufData Int Word8))
      F.forM_ [0 .. numPixels - 1] $ \i -> do
        let cvt :: Double -> Word8
            cvt x
              | x < 0.0039 = 0
              | x > 0.996 = 255
              | otherwise = fromIntegral (floor (x * 256 - 1) `mod` 256 :: Int)
        if fromIntegral (i `mod` maxCursorWidth) < cursize
          && fromIntegral (i `div` maxCursorWidth) < cursize
          then do
            writeArray pbData (4 * i) (cvt r)
            writeArray pbData (4 * i + 1) (cvt g)
            writeArray pbData (4 * i + 2) (cvt b)
            writeArray pbData (4 * i + 3) (cvt a)
          else do
            writeArray pbData (4 * i) 0
            writeArray pbData (4 * i + 1) 0
            writeArray pbData (4 * i + 2) 0
            writeArray pbData (4 * i + 3) 0

      Gtk.drawWindowSetCursor win . Just
        =<< Gtk.cursorNewFromPixbuf dpy pb (floor cursize `div` 2) (floor cursize `div` 2)
      return (UsrEv ActionOrdered)
