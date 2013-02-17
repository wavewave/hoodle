{-# LANGUAGE TypeOperators, GADTs, ScopedTypeVariables, Rank2Types  #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Accessor 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Accessor where

import           Control.Applicative
import           Control.Category
import           Control.Lens
import           Control.Monad hiding (mapM_, forM_)
import qualified Control.Monad.State as St hiding (mapM_, forM_)
import           Control.Monad.Trans
import           Data.Foldable
import qualified Data.IntMap as M
import           Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk as Gtk (set)
-- from hoodle-platform 
import           Data.Hoodle.Generic
import           Data.Hoodle.Select
import           Graphics.Hoodle.Render.Type
-- from this package
import           Hoodle.GUI.Menu
import           Hoodle.ModelAction.Layer 
import           Hoodle.Type
import           Hoodle.Type.Alias
import           Hoodle.View.Coordinate
import           Hoodle.Type.PageArrangement
--
import           Prelude hiding ((.),id,mapM_)


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
  return $ fmap4CvsInfoBox (\x->getCurrentPageFromHoodleModeState x hdlmodst) cinfobox

-- | 
getCurrentPageCvsId :: CanvasId -> MainCoroutine (Page EditMode) 
getCurrentPageCvsId cid = do 
  xstate <- St.get 
  let hdlmodst = view hoodleModeState xstate
      cinfobox = getCanvasInfo cid xstate 
  return $ fmap4CvsInfoBox (\c->getCurrentPageFromHoodleModeState c hdlmodst) cinfobox


-- | 
getCurrentPageEitherFromHoodleModeState :: 
  (ViewMode a) => 
  CanvasInfo a -> HoodleModeState -> Either (Page EditMode) (Page SelectMode)
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

-- | 
changeCurrentCanvasId :: CanvasId -> MainCoroutine HoodleState 
changeCurrentCanvasId cid = do 
    xstate1 <- St.get
    maybe (return xstate1) 
          (\xst -> do St.put xst 
                      return xst)
          (setCurrentCanvasId cid xstate1)
    reflectUI
    St.get     
    
{-    xst <- St.get
    let cinfo = view currentCanvasInfo xst               
        ui = view gtkUIManager xst                      
    reflectUI ui cinfo
    return xst -}

-- | apply an action to all canvases 
applyActionToAllCVS :: (CanvasId -> MainCoroutine ()) -> MainCoroutine () 
applyActionToAllCVS action = do 
  xstate <- St.get 
  let cinfoMap  = getCanvasInfoMap xstate
      keys = M.keys cinfoMap 
  forM_ keys action

{- reflectUI_ :: MainCoroutine () 
reflectUI_ = do 
  let ui = view gtkUIMag
    let cinfo = view currentCanvasInfo xst               
        ui = view gtkUIManager xst                      
    reflectUI ui cinfo
      
  reflectUI -}

-- | reflect UI for current canvas info 
reflectUI :: MainCoroutine ()
reflectUI = do 
    xstate <- St.get
    let cinfobox = view currentCanvasInfo xstate 
        ui = view gtkUIManager xstate       
    let mconnid = view pageModeSignal xstate
    liftIO $ maybe (return ()) signalBlock mconnid 
    agr <- liftIO $ uiManagerGetActionGroups ui
    ra1 <- maybe (error "reflectUI") return =<< 
             liftIO (actionGroupGetAction (head agr) "ONEPAGEA")
    pma <- maybe (error "reflectUI") return =<<  
             liftIO (actionGroupGetAction (head agr) "PENFINEA")
    let wra1 = castToRadioAction ra1 
        wpma = castToRadioAction wpma
    
    
    selectBoxAction (pgmodupdate_s wra1) (pgmodupdate_c wra1) cinfobox 

    -- liftIO $ actionSetSensitive wpma False
    -- boxAction (penmodupdate xstate wpma) cinfobox
    -- liftIO $ actionSetSensitive wpma True 
    
    liftIO $ maybe (return ()) signalUnblock mconnid 
    return ()
  where (#) :: a -> (a -> b) -> b 
        (#) = flip ($)
        pgmodupdate_s wra1 _cinfo = do
          liftIO $ Gtk.set wra1 [radioActionCurrentValue := 1 ] 
        pgmodupdate_c wra1 _cinfo = do
          liftIO $ Gtk.set wra1 [radioActionCurrentValue := 0 ] 
            
        penmodupdate xst wpma _cinfo = do 
          let pmodv = hoodleModeStateEither (view hoodleModeState xst) #  
                either (\_ -> (penType2Int. Left .view (penInfo.penType)) xst)
                       (\_ -> (penType2Int. Right .view (selectInfo.selectType)) xst)
          liftIO $ Gtk.set wpma [radioActionCurrentValue := pmodv ] 
          
            
{-
          hoodleModeStateEither xst
            ViewAppendState _ -> do 

penType2Int 
              let     view (penInfo.penType) xst
          liftIO $ Gtk.set wpma [radioActionCurrentValue := pmodv ] -}
 

-- | 
printViewPortBBox :: CanvasId -> MainCoroutine ()
printViewPortBBox cid = do 
  cvsInfo <- return . getCanvasInfo cid =<< St.get 
  liftIO $ putStrLn $ show (unboxGet (viewInfo.pageArrangement.viewPortBBox) cvsInfo)

-- | 
printViewPortBBoxAll :: MainCoroutine () 
printViewPortBBoxAll = do 
  xstate <- St.get 
  let cmap = getCanvasInfoMap xstate
      cids = M.keys cmap
  mapM_ printViewPortBBox cids 

-- | 
printViewPortBBoxCurr :: MainCoroutine ()
printViewPortBBoxCurr = do 
  cvsInfo <- return . view currentCanvasInfo =<< St.get 
  liftIO $ putStrLn $ show (unboxGet (viewInfo.pageArrangement.viewPortBBox) cvsInfo)

-- | 
printModes :: CanvasId -> MainCoroutine ()
printModes cid = do 
  cvsInfo <- return . getCanvasInfo cid =<< St.get 
  liftIO $ printCanvasMode cid cvsInfo

-- |
printCanvasMode :: CanvasId -> CanvasInfoBox -> IO ()
printCanvasMode cid cvsInfo = do 
  let zmode = unboxGet (viewInfo.zoomMode) cvsInfo
      f :: PageArrangement a -> String 
      f (SingleArrangement _ _ _) = "SingleArrangement"
      f (ContinuousArrangement _ _ _ _) = "ContinuousArrangement"
      g :: CanvasInfo a -> String 
      g cinfo = f . view (viewInfo.pageArrangement) $ cinfo
      arrmode :: String 
      arrmode = boxAction g cvsInfo  
      incid = unboxGet canvasId cvsInfo 
  putStrLn $ show (cid,incid,zmode,arrmode)

-- |
printModesAll :: MainCoroutine () 
printModesAll = do 
  xstate <- St.get 
  let cmap = getCanvasInfoMap xstate
      cids = M.keys cmap
  mapM_ printModes cids 

-- | 
getCanvasGeometryCvsId :: CanvasId -> HoodleState -> IO CanvasGeometry 
getCanvasGeometryCvsId cid xstate = do 
  let cinfobox = getCanvasInfo cid xstate
      cpn = PageNum . unboxGet currentPageNum $ cinfobox 
      canvas = unboxGet drawArea cinfobox
      fsingle :: (ViewMode a) => CanvasInfo a -> IO CanvasGeometry 
      fsingle = flip (makeCanvasGeometry cpn) canvas 
                . view (viewInfo.pageArrangement) 
  boxAction fsingle cinfobox

-- |
getGeometry4CurrCvs :: HoodleState -> IO CanvasGeometry 
getGeometry4CurrCvs xstate = do 
  let cinfobox = view currentCanvasInfo xstate
      cpn = PageNum . unboxGet currentPageNum $ cinfobox 
      canvas = unboxGet drawArea cinfobox
      fsingle :: (ViewMode a) => CanvasInfo a -> IO CanvasGeometry 
      fsingle = flip (makeCanvasGeometry cpn) canvas 
                . view (viewInfo.pageArrangement) 
  boxAction fsingle cinfobox
  
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



