{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hoodle.Accessor where

import Control.Applicative
import Control.Concurrent.STM (readTVarIO)
import Control.Lens ((.~), Lens, Simple, set, view)
import Control.Monad hiding (forM_, mapM_)
import Control.Monad.IO.Class
import qualified Control.Monad.State as St hiding (forM_, mapM_)
import Data.Foldable
import Data.Hoodle.Generic
import Data.Hoodle.Select
import qualified Data.IntMap as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Graphics.Hoodle.Render.Type
import qualified Graphics.UI.Gtk as Gtk
import Hoodle.ModelAction.Layer
import Hoodle.Type
import Hoodle.Type.Alias
import Hoodle.Type.PageArrangement
import Hoodle.View.Coordinate
import Prelude hiding (mapM_)

-- | update state
updateXState :: (HoodleState -> MainCoroutine HoodleState) -> MainCoroutine ()
updateXState action = St.put =<< action =<< St.get

-- | update unitHoodle
updateUhdl :: (UnitHoodle -> MainCoroutine UnitHoodle) -> MainCoroutine ()
updateUhdl action = do
  xst <- St.get
  let uhdl = view (unitHoodles . currentUnit) xst
  uhdl' <- action uhdl
  St.put (set (unitHoodles . currentUnit) uhdl' xst)

-- | update unitHoodle
pureUpdateUhdl :: (UnitHoodle -> UnitHoodle) -> MainCoroutine ()
pureUpdateUhdl func = do
  xst <- St.get
  let uhdl = (func . view (unitHoodles . currentUnit)) xst
  St.put ((unitHoodles . currentUnit .~ uhdl) xst)

-- |
getPenType :: MainCoroutine PenType
getPenType = view (penInfo . penType) <$> St.get

-- |
getCurrentPageCurr :: MainCoroutine (Page EditMode)
getCurrentPageCurr = do
  xstate <- St.get
  let uhdl = view (unitHoodles . currentUnit) xstate
      hdlmodst = view hoodleModeState uhdl
      cinfobox = view currentCanvasInfo uhdl
  return $ forBoth' unboxBiAct (flip getCurrentPageFromHoodleModeState hdlmodst) cinfobox

-- |
getCurrentPageCvsId :: CanvasId -> MainCoroutine (Page EditMode)
getCurrentPageCvsId cid = do
  xstate <- St.get
  let uhdl = view (unitHoodles . currentUnit) xstate
      hdlmodst = view hoodleModeState uhdl
      cinfobox = getCanvasInfo cid uhdl
  return $ forBoth' unboxBiAct (flip getCurrentPageFromHoodleModeState hdlmodst) cinfobox

-- |
getCurrentPageEitherFromHoodleModeState ::
  CanvasInfo a -> HoodleModeState -> Either (Page EditMode) (Page SelectMode)
getCurrentPageEitherFromHoodleModeState cinfo hdlmodst =
  let cpn = view currentPageNum cinfo
      page = getCurrentPageFromHoodleModeState cinfo hdlmodst
   in case hdlmodst of
        ViewAppendState _hdl -> Left page
        SelectState thdl ->
          case view gselSelected thdl of
            Nothing -> Left page
            Just (n, tpage) ->
              if cpn == n
                then Right tpage
                else Left page

-- |
rItmsInCurrLyr :: MainCoroutine [RItem]
rItmsInCurrLyr = return . view gitems . getCurrentLayer =<< getCurrentPageCurr

-- |
otherCanvas :: UnitHoodle -> [Int]
otherCanvas = M.keys . view cvsInfoMap

-- | apply an action to all canvases
applyActionToAllCVS :: (CanvasId -> MainCoroutine ()) -> MainCoroutine ()
applyActionToAllCVS action = do
  xstate <- St.get
  let cinfoMap = view (unitHoodles . currentUnit . cvsInfoMap) xstate
      keys = M.keys cinfoMap
  forM_ keys action

-- |
getCanvasGeometryCvsId :: CanvasId -> UnitHoodle -> IO CanvasGeometry
getCanvasGeometryCvsId cid uhdl = do
  let cinfobox = getCanvasInfo cid uhdl
      cpn = PageNum . view (unboxLens currentPageNum) $ cinfobox
      canvas = view (unboxLens drawArea) cinfobox
      fsingle :: CanvasInfo a -> IO CanvasGeometry
      fsingle =
        flip (makeCanvasGeometry cpn) canvas
          . view (viewInfo . pageArrangement)
  forBoth' unboxBiAct fsingle cinfobox

-- |
getGeometry4CurrCvs :: UnitHoodle -> IO CanvasGeometry
getGeometry4CurrCvs uhdl = do
  let cinfobox = view currentCanvasInfo uhdl
      cpn = PageNum . view (unboxLens currentPageNum) $ cinfobox
      canvas = view (unboxLens drawArea) cinfobox
      fsingle :: CanvasInfo a -> IO CanvasGeometry
      fsingle =
        flip (makeCanvasGeometry cpn) canvas
          . view (viewInfo . pageArrangement)
  forBoth' unboxBiAct fsingle cinfobox

-- | set toggle UI button to the corresponding HoodleState
lensSetToggleUIForFlag ::
  String ->
  -- | lens for flag
  Simple Lens HoodleState Bool ->
  HoodleState ->
  IO Bool
lensSetToggleUIForFlag toggleid lensforflag xstate =
  let b = view lensforflag xstate in setToggleUIForFlag toggleid b xstate

-- | set toggle UI button to the corresponding HoodleState
setToggleUIForFlag :: String -> Bool -> HoodleState -> IO Bool
setToggleUIForFlag toggleid b xstate = do
  let ui = view gtkUIManager xstate
  agr <- Gtk.uiManagerGetActionGroups ui >>= \x ->
    case x of
      [] -> error "No action group? "
      y : _ -> return y
  togglea <- Gtk.actionGroupGetAction agr toggleid >>= \(Just x) ->
    return (Gtk.castToToggleAction x)
  Gtk.toggleActionSetActive togglea b
  return b

-- |
renderCache :: MainCoroutine RenderCache
renderCache = (view renderCacheVar <$> St.get) >>= liftIO . readTVarIO

-- |
getHoodleFilePath :: UnitHoodle -> Maybe FilePath
getHoodleFilePath = fileStore2Maybe . view (hoodleFileControl . hoodleFileName)

-- |
fileStore2Maybe :: FileStore -> Maybe FilePath
fileStore2Maybe (LocalDir Nothing) = Nothing
fileStore2Maybe (LocalDir (Just filename)) = Just filename
fileStore2Maybe (TempDir filename) = Just filename

-- |
getHoodleID :: UnitHoodle -> T.Text
getHoodleID = TE.decodeUtf8 . view ghoodleID . getHoodle
