{-# LANGUAGE TypeOperators, GADTs, ScopedTypeVariables  #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.HXournal.Accessor 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.HXournal.Accessor where

import Application.HXournal.Type
import Application.HXournal.View.Draw 
import Application.HXournal.ModelAction.Page
import Control.Applicative
import Control.Monad
import qualified Control.Monad.State as St
import Control.Monad.Trans
import Control.Category
import qualified Data.IntMap as M
import Data.Label
import Prelude hiding ((.),id)
import Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk as Gtk (get,set)
import Data.Xournal.BBox
import Data.Xournal.Generic
import Application.HXournal.Util
import Application.HXournal.ModelAction.Layer 
import Application.HXournal.Type.Alias
import Application.HXournal.Type.Event
import Application.HXournal.Type.PageArrangement
import Application.HXournal.Type.Coroutine
import Application.HXournal.View.Coordinate
import Control.Monad.Coroutine 
import Control.Monad.Coroutine.SuspensionFunctors

-- | get HXournalState 

getSt :: MainCoroutine HXournalState 
getSt = lift St.get

-- | put HXournalState

putSt :: HXournalState -> MainCoroutine () 
putSt = lift . St.put

-- | update state

updateXState :: (HXournalState -> MainCoroutine HXournalState) -> MainCoroutine ()
updateXState action = putSt =<< action =<< getSt 


-- | 

getPenType :: Iteratee MyEvent XournalStateIO PenType
getPenType = get (penType.penInfo) <$> lift (St.get)
      
-- | 

getAllStrokeBBoxInCurrentPage :: MainCoroutine [StrokeBBox] 
getAllStrokeBBoxInCurrentPage = do 
  xstate <- getSt 
  case get currentCanvasInfo xstate of
    CanvasInfoBox currCvsInfo -> 
      let pagebbox = getPage currCvsInfo
      in  return [s| l <- gToList (get g_layers pagebbox), s <- get g_bstrokes l ]
  

getAllStrokeBBoxInCurrentLayer :: MainCoroutine [StrokeBBox] 
getAllStrokeBBoxInCurrentLayer = do 
  xstate <- getSt 
  case get currentCanvasInfo xstate of 
    CanvasInfoBox currCvsInfo -> do 
      let pagebbox = getPage currCvsInfo
          (mcurrlayer, _currpage) = getCurrentLayerOrSet pagebbox
          currlayer = maybe (error "getAllStrokeBBoxInCurrentLayer") id mcurrlayer
      return (get g_bstrokes currlayer)
      
otherCanvas :: HXournalState -> [Int] 
otherCanvas = M.keys . getCanvasInfoMap 


-- | 

changeCurrentCanvasId :: CanvasId -> MainCoroutine HXournalState 
changeCurrentCanvasId cid = do 
    xstate1 <- getSt
    maybe (return xstate1) 
          (\xst -> do putSt xst 
                      liftIO $ putStrLn "------------------------"
                      printModesAll 
                      liftIO $ putStrLn $ "current id " ++ show (getCurrentCanvasId xst) ++ " cid = " ++ ( show cid )
                      return xst)
          (setCurrentCanvasId cid xstate1)
    xst <- getSt
    let cinfo = get currentCanvasInfo xst               
        ui = get gtkUIManager xst                      
    liftIO $ putStrLn $ " cinfo cid = " ++ show (unboxGet canvasId cinfo)
    reflectUI ui cinfo
    return xst
    
{-
    let maction = do  
    (>>=) (return . M.lookup cid . get canvasInfoMap $ xstate1) $
     maybe (return xstate1) 
           (\cinfo -> do 
               let nst = set currentCanvasInfo cinfo 
                         . set currentCanvasId cid $ xstate1 
                   ui = get gtkUIManager nst 
               putSt nst >> return nst
           )
    -- set currentCanvas (cid,cinfo) $ xstate1 
-}


-- | reflect UI for current canvas info 

reflectUI :: UIManager -> CanvasInfoBox -> MainCoroutine ()
reflectUI ui cinfobox = do 
    xstate <- getSt
    let mconnid = get pageModeSignal xstate
    liftIO $ maybe (return ()) signalBlock mconnid 
    agr <- liftIO $ uiManagerGetActionGroups ui
    Just ra1 <- liftIO $ actionGroupGetAction (head agr) "ONEPAGEA"
    let wra1 = castToRadioAction ra1 
    selectBoxAction (fsingle ra1) (fcont ra1) cinfobox 
    liftIO $ maybe (return ()) signalUnblock mconnid 
    return ()
  where fsingle ra1 cinfo = do
          let wra1 = castToRadioAction ra1           
          liftIO $ Gtk.set wra1 [radioActionCurrentValue := 1 ] 
        fcont ra1 cinfo = do
          let wra1 = castToRadioAction ra1 
          -- liftIO $ wra1 `on` radioActionChanged $ const (putStrLn "hellowworld2" >> return ())
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
{-  let zmode = unboxGet (zoomMode.viewInfo) cvsInfo
      f :: PageArrangement a -> String 
      f (SingleArrangement _ _ _) = "SingleArrangement"
      f (ContinuousSingleArrangement _ _ _ _) = "ContinuousSingleArrangement"

      g :: CanvasInfo a -> String 
      g cinfo = f . get (pageArrangement.viewInfo) $ cinfo
      
      arrmode :: String 
      arrmode = boxAction g cvsInfo  
      
      incid = unboxGet canvasId cvsInfo 
  liftIO $ putStrLn $ show (cid,incid,zmode,arrmode) -}

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


printModesAll :: MainCoroutine () 
printModesAll = do 
  xstate <- getSt 
  let cmap = getCanvasInfoMap xstate
      cids = M.keys cmap
  mapM_ printModes cids 


-- | 

unboxGetPage :: CanvasInfoBox -> (Page EditMode) 
unboxGetPage = either id (gcast :: Page SelectMode -> Page EditMode) . unboxGet currentPage

-- | 

getCanvasGeometryCvsId :: CanvasId -> HXournalState -> IO CanvasGeometry 
getCanvasGeometryCvsId cid xstate = do 
  let cinfobox = getCanvasInfo cid xstate
      page = unboxGetPage cinfobox
      cpn = PageNum . unboxGet currentPageNum $ cinfobox 
      canvas = unboxGet drawArea cinfobox
      xojstate = get xournalstate xstate 
      fsingle :: (ViewMode a) => CanvasInfo a -> IO CanvasGeometry 
      fsingle = flip (makeCanvasGeometry EditMode (cpn,page)) canvas 
                . get (pageArrangement.viewInfo) 
  boxAction fsingle cinfobox

-- |

getCanvasGeometry :: HXournalState -> IO CanvasGeometry 
getCanvasGeometry xstate = do 
  let cinfobox = get currentCanvasInfo xstate
      page = unboxGetPage cinfobox
      cpn = PageNum . unboxGet currentPageNum $ cinfobox 
      canvas = unboxGet drawArea cinfobox
      xojstate = get xournalstate xstate 
      fsingle :: (ViewMode a) => CanvasInfo a -> IO CanvasGeometry 
      fsingle = flip (makeCanvasGeometry EditMode (cpn,page)) canvas 
                . get (pageArrangement.viewInfo) 

  boxAction fsingle cinfobox
  

{-
getCanvasGeometry :: CanvasInfo SinglePage -> MainCoroutine CanvasPageGeometry 
getCanvasGeometry cinfo = do 
    let canvas = get drawArea cinfo
        page = getPage cinfo
        (x0,y0) = bbox_upperleft . unViewPortBBox . get (viewPortBBox.pageArrangement.viewInfo) $ cinfo
    liftIO (getCanvasPageGeometry canvas page (x0,y0))
-}








