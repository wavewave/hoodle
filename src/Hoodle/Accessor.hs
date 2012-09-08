{-# LANGUAGE TypeOperators, GADTs, ScopedTypeVariables  #-}

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
import           Control.Monad hiding (mapM_)
import qualified Control.Monad.State as St hiding (mapM_)
import           Control.Monad.Trans
import           Data.Foldable
import qualified Data.IntMap as M
-- import           Data.Label
import           Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk as Gtk (set)
-- from hoodle-platform 
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
-- from this package
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
  case cinfobox of 
    CanvasInfoBox cinfo -> return (getCurrentPageFromHoodleModeState cinfo hdlmodst)

-- | 
getCurrentPageCvsId :: CanvasId -> MainCoroutine (Page EditMode) 
getCurrentPageCvsId cid = do 
  xstate <- St.get 
  let hdlmodst = view hoodleModeState xstate
      cinfobox = getCanvasInfo cid xstate 
  case cinfobox of 
    CanvasInfoBox cinfo -> return (getCurrentPageFromHoodleModeState cinfo hdlmodst)

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
           case view g_selectSelected thdl of 
             Nothing -> Left page
             Just (n,tpage) -> if cpn == n 
                                 then Right tpage
                                 else Left page

-- | 

getAllStrokeBBoxInCurrentPage :: MainCoroutine [StrokeBBox] 
getAllStrokeBBoxInCurrentPage = do 
  page <- getCurrentPageCurr
  return [s| l <- gToList (view g_layers page), s <- view g_bstrokes l ]
  
-- | 

getAllStrokeBBoxInCurrentLayer :: MainCoroutine [StrokeBBox] 
getAllStrokeBBoxInCurrentLayer = do 
  page <- getCurrentPageCurr
  let (mcurrlayer, _currpage) = getCurrentLayerOrSet page
      currlayer = maybe (error "getAllStrokeBBoxInCurrentLayer") id mcurrlayer
  return (view g_bstrokes currlayer)
      
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
    xst <- St.get
    let cinfo = view currentCanvasInfo xst               
        ui = view gtkUIManager xst                      
    reflectUI ui cinfo
    return xst

-- | reflect UI for current canvas info 

reflectUI :: UIManager -> CanvasInfoBox -> MainCoroutine ()
reflectUI ui cinfobox = do 
    xstate <- St.get
    let mconnid = view pageModeSignal xstate
    liftIO $ maybe (return ()) signalBlock mconnid 
    agr <- liftIO $ uiManagerGetActionGroups ui
    Just ra1 <- liftIO $ actionGroupGetAction (head agr) "ONEPAGEA"
    selectBoxAction (fsingle ra1) (fcont ra1) cinfobox 
    liftIO $ maybe (return ()) signalUnblock mconnid 
    return ()
  where fsingle ra1 _cinfo = do
          let wra1 = castToRadioAction ra1           
          liftIO $ Gtk.set wra1 [radioActionCurrentValue := 1 ] 
        fcont ra1 _cinfo = do
          liftIO $ Gtk.set (castToRadioAction ra1) [radioActionCurrentValue := 0 ] 
  
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
      f (ContinuousSingleArrangement _ _ _ _) = "ContinuousSingleArrangement"
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
  
-- | 

bbox4AllStrokes :: (Foldable t, Functor t) => t StrokeBBox -> ULMaybe BBox 
bbox4AllStrokes = unUnion . fold . fmap (Union . Middle . strokebbox_bbox)


