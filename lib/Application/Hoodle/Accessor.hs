{-# LANGUAGE TypeOperators, GADTs, ScopedTypeVariables  #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.Hoodle.Accessor 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.Hoodle.Accessor where

import Application.Hoodle.Type
import Control.Applicative
import Control.Monad hiding (mapM_)
import qualified Control.Monad.State as St hiding (mapM_)
import Control.Monad.Trans
import Control.Category
import qualified Data.IntMap as M
import Data.Label
import Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk as Gtk (set)
import Data.Foldable
import Data.Monoid
import Data.Xournal.BBox
import Data.Xournal.Generic
import Application.Hoodle.ModelAction.Layer 
import Application.Hoodle.Type.Alias
import Application.Hoodle.Type.PageArrangement
import Application.Hoodle.View.Coordinate
import Prelude hiding ((.),id,mapM_)

-- | get HoodleState 

getSt :: MainCoroutine HoodleState 
getSt = lift St.get

-- | put HoodleState

putSt :: HoodleState -> MainCoroutine () 
putSt = lift . St.put

-- | update state

updateXState :: (HoodleState -> MainCoroutine HoodleState) -> MainCoroutine ()
updateXState action = putSt =<< action =<< getSt 

-- | 

getPenType :: MainCoroutine PenType 
getPenType = get (penType.penInfo) <$> lift (St.get)
      
-- | 

getCurrentPageCurr :: MainCoroutine (Page EditMode) 
getCurrentPageCurr = do 
  xstate <- getSt 
  let xojstate = get xournalstate xstate
      cinfobox = get currentCanvasInfo xstate 
  case cinfobox of 
    CanvasInfoBox cinfo -> return (getCurrentPageFromXojState cinfo xojstate)

-- | 

getCurrentPageCvsId :: CanvasId -> MainCoroutine (Page EditMode) 
getCurrentPageCvsId cid = do 
  xstate <- getSt 
  let xojstate = get xournalstate xstate
      cinfobox = getCanvasInfo cid xstate 
  case cinfobox of 
    CanvasInfoBox cinfo -> return (getCurrentPageFromXojState cinfo xojstate)

-- | 
    
getCurrentPageEitherFromXojState :: (ViewMode a) => 
                                    CanvasInfo a -> XournalState  
                                    -> Either (Page EditMode) (Page SelectMode)
getCurrentPageEitherFromXojState cinfo xojstate =  
    let cpn = get currentPageNum cinfo 
        page = getCurrentPageFromXojState cinfo xojstate
    in case xojstate of 
         ViewAppendState _xoj -> Left page
         SelectState txoj ->  
           case get g_selectSelected txoj of 
             Nothing -> Left page
             Just (n,tpage) -> if cpn == n 
                                 then Right tpage
                                 else Left page

-- | 

getAllStrokeBBoxInCurrentPage :: MainCoroutine [StrokeBBox] 
getAllStrokeBBoxInCurrentPage = do 
  page <- getCurrentPageCurr
  return [s| l <- gToList (get g_layers page), s <- get g_bstrokes l ]
  
-- | 

getAllStrokeBBoxInCurrentLayer :: MainCoroutine [StrokeBBox] 
getAllStrokeBBoxInCurrentLayer = do 
  page <- getCurrentPageCurr
  let (mcurrlayer, _currpage) = getCurrentLayerOrSet page
      currlayer = maybe (error "getAllStrokeBBoxInCurrentLayer") id mcurrlayer
  return (get g_bstrokes currlayer)
      
-- |

otherCanvas :: HoodleState -> [Int] 
otherCanvas = M.keys . getCanvasInfoMap 

-- | 

changeCurrentCanvasId :: CanvasId -> MainCoroutine HoodleState 
changeCurrentCanvasId cid = do 
    xstate1 <- getSt
    maybe (return xstate1) 
          (\xst -> do putSt xst 
                      return xst)
          (setCurrentCanvasId cid xstate1)
    xst <- getSt
    let cinfo = get currentCanvasInfo xst               
        ui = get gtkUIManager xst                      
    reflectUI ui cinfo
    return xst

-- | reflect UI for current canvas info 

reflectUI :: UIManager -> CanvasInfoBox -> MainCoroutine ()
reflectUI ui cinfobox = do 
    xstate <- getSt
    let mconnid = get pageModeSignal xstate
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
  cvsInfo <- return . getCanvasInfo cid =<< getSt 
  liftIO $ putStrLn $ show (unboxGet (viewPortBBox.pageArrangement.viewInfo) cvsInfo)

-- | 

printViewPortBBoxAll :: MainCoroutine () 
printViewPortBBoxAll = do 
  xstate <- getSt 
  let cmap = getCanvasInfoMap xstate
      cids = M.keys cmap
  mapM_ printViewPortBBox cids 

-- | 
  
printViewPortBBoxCurr :: MainCoroutine ()
printViewPortBBoxCurr = do 
  cvsInfo <- return . get currentCanvasInfo =<< getSt 
  liftIO $ putStrLn $ show (unboxGet (viewPortBBox.pageArrangement.viewInfo) cvsInfo)

-- | 
  
printModes :: CanvasId -> MainCoroutine ()
printModes cid = do 
  cvsInfo <- return . getCanvasInfo cid =<< getSt 
  liftIO $ printCanvasMode cid cvsInfo

-- |

printCanvasMode :: CanvasId -> CanvasInfoBox -> IO ()
printCanvasMode cid cvsInfo = do 
  let zmode = unboxGet (zoomMode.viewInfo) cvsInfo
      f :: PageArrangement a -> String 
      f (SingleArrangement _ _ _) = "SingleArrangement"
      f (ContinuousSingleArrangement _ _ _ _) = "ContinuousSingleArrangement"
      g :: CanvasInfo a -> String 
      g cinfo = f . get (pageArrangement.viewInfo) $ cinfo
      arrmode :: String 
      arrmode = boxAction g cvsInfo  
      incid = unboxGet canvasId cvsInfo 
  putStrLn $ show (cid,incid,zmode,arrmode)

-- |
  
printModesAll :: MainCoroutine () 
printModesAll = do 
  xstate <- getSt 
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
                . get (pageArrangement.viewInfo) 
  boxAction fsingle cinfobox

-- |

getGeometry4CurrCvs :: HoodleState -> IO CanvasGeometry 
getGeometry4CurrCvs xstate = do 
  let cinfobox = get currentCanvasInfo xstate
      cpn = PageNum . unboxGet currentPageNum $ cinfobox 
      canvas = unboxGet drawArea cinfobox
      fsingle :: (ViewMode a) => CanvasInfo a -> IO CanvasGeometry 
      fsingle = flip (makeCanvasGeometry cpn) canvas 
                . get (pageArrangement.viewInfo) 
  boxAction fsingle cinfobox
  
-- | 

bbox4AllStrokes :: (Foldable t, Functor t) => t StrokeBBox -> ULMaybe BBox 
bbox4AllStrokes = unUnion . fold . fmap (Union . Middle . strokebbox_bbox)


