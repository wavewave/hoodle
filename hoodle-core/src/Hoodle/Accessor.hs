{-# LANGUAGE TypeOperators, GADTs, ScopedTypeVariables, Rank2Types  #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Accessor 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Accessor where

import           Control.Applicative
import           Control.Lens (Simple,Lens,view,set)
import           Control.Monad hiding (mapM_, forM_)
import qualified Control.Monad.State as St hiding (mapM_, forM_)
import           Control.Monad.Trans
import           Data.Foldable
import qualified Data.IntMap as M
import           Graphics.UI.Gtk hiding (get,set)
-- from hoodle-platform 
import           Data.Hoodle.Generic
import           Data.Hoodle.Select
import           Graphics.Hoodle.Render.Type
-- from this package
-- import           Hoodle.GUI.Menu
-- import           Hoodle.GUI.Reflect
import           Hoodle.ModelAction.Layer 
import           Hoodle.Type
import           Hoodle.Type.Alias
import           Hoodle.View.Coordinate
import           Hoodle.Type.PageArrangement
--
import           Prelude hiding (mapM_)

-- | update state
updateXState :: (HoodleState -> MainCoroutine HoodleState) -> MainCoroutine ()
updateXState action = St.put =<< action =<< St.get 

-- | 
getPenType :: MainCoroutine PenType 
getPenType = view (penInfo.penType) <$> St.get
      
-- | 
getCurrentPageCurr :: MainCoroutine (Page EditMode) 
getCurrentPageCurr = do 
  xstate <- St.get 
  let hdlmodst = view hoodleModeState xstate
      cinfobox = view currentCanvasInfo xstate 
  return $ forBoth' unboxBiAct (flip getCurrentPageFromHoodleModeState hdlmodst) cinfobox

-- | 
getCurrentPageCvsId :: CanvasId -> MainCoroutine (Page EditMode) 
getCurrentPageCvsId cid = do 
  xstate <- St.get 
  let hdlmodst = view hoodleModeState xstate
      cinfobox = getCanvasInfo cid xstate 
  return $ forBoth' unboxBiAct (flip getCurrentPageFromHoodleModeState hdlmodst) cinfobox


-- | 
getCurrentPageEitherFromHoodleModeState 
  :: CanvasInfo a -> HoodleModeState -> Either (Page EditMode) (Page SelectMode)
getCurrentPageEitherFromHoodleModeState cinfo hdlmodst =  
    let cpn = view currentPageNum cinfo 
        page = getCurrentPageFromHoodleModeState cinfo hdlmodst
    in case hdlmodst of 
         ViewAppendState _hdl -> Left page
         SelectState thdl ->  
           case view gselSelected thdl of 
             Nothing -> Left page
             Just (n,tpage) -> if cpn == n 
                                 then Right tpage
                                 else Left page

-- | 
rItmsInCurrLyr :: MainCoroutine [RItem] 
rItmsInCurrLyr = return . view gitems . getCurrentLayer =<< getCurrentPageCurr
      
-- |
otherCanvas :: HoodleState -> [Int] 
otherCanvas = M.keys . getCanvasInfoMap 


-- | apply an action to all canvases 
applyActionToAllCVS :: (CanvasId -> MainCoroutine ()) -> MainCoroutine () 
applyActionToAllCVS action = do 
  xstate <- St.get 
  let cinfoMap  = getCanvasInfoMap xstate
      keys = M.keys cinfoMap 
  forM_ keys action

-- | 
getCanvasGeometryCvsId :: CanvasId -> HoodleState -> IO CanvasGeometry 
getCanvasGeometryCvsId cid xstate = do 
  let cinfobox = getCanvasInfo cid xstate
      cpn = PageNum . view (unboxLens currentPageNum) $ cinfobox 
      canvas = view (unboxLens drawArea) cinfobox
      fsingle :: CanvasInfo a -> IO CanvasGeometry 
      fsingle = flip (makeCanvasGeometry cpn) canvas 
                . view (viewInfo.pageArrangement) 
  forBoth' unboxBiAct fsingle cinfobox

-- |
getGeometry4CurrCvs :: HoodleState -> IO CanvasGeometry 
getGeometry4CurrCvs xstate = do 
  let cinfobox = view currentCanvasInfo xstate
      cpn = PageNum . view (unboxLens currentPageNum) $ cinfobox 
      canvas = view (unboxLens drawArea) cinfobox
      fsingle :: CanvasInfo a -> IO CanvasGeometry 
      fsingle = flip (makeCanvasGeometry cpn) canvas 
                . view (viewInfo.pageArrangement) 
  forBoth' unboxBiAct fsingle cinfobox

  
-- | update flag in HoodleState when corresponding toggle UI changed 
updateFlagFromToggleUI :: String  -- ^ UI toggle button id
                       -> Simple Lens HoodleState Bool -- ^ lens for flag 
                       -> MainCoroutine Bool
updateFlagFromToggleUI toggleid lensforflag = do 
  xstate <- St.get 
  let ui = view gtkUIManager xstate 
  agr <- liftIO ( uiManagerGetActionGroups ui >>= \x ->
                    case x of 
                      [] -> error "No action group? "
                      y:_ -> return y )
  togglea <- liftIO (actionGroupGetAction agr toggleid) 
             >>= maybe (error "updateFlagFromToggleUI") 
                       (return . castToToggleAction)
  b <- liftIO $ toggleActionGetActive togglea
  St.modify (set lensforflag b) 
  return b 

-- | set toggle UI button to the corresponding HoodleState 
setToggleUIForFlag :: String 
                   -> Simple Lens HoodleState Bool -- ^ lens for flag
                   -> HoodleState 
                   -> IO Bool 
setToggleUIForFlag toggleid lensforflag xstate = do 
  let ui = view gtkUIManager xstate 
      b = view lensforflag xstate 
  agr <- uiManagerGetActionGroups ui >>= \x ->
           case x of 
             [] -> error "No action group? "
             y:_ -> return y 
  togglea <- actionGroupGetAction agr toggleid >>= \(Just x) -> 
                return (castToToggleAction x) 
  toggleActionSetActive togglea b
  return b 



