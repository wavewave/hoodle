{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.GUI 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.GUI where

import Hoodle.Type.Coroutine
import Hoodle.Type.XournalState 
import Hoodle.Type.Event
import Hoodle.Type.Canvas
import Hoodle.Type.Undo 
import qualified Data.IntMap as M

import Hoodle.Coroutine.Callback

import Hoodle.Config 
import Hoodle.Device
import Hoodle.Coroutine
-- import Hoodle.GUI.Menu
import Hoodle.ModelAction.File 
import Hoodle.ModelAction.Window
import Hoodle.Script.Hook
import Graphics.UI.Gtk hiding (get,set)
import Control.Monad.Trans 
import Data.IORef
import Data.Maybe
import Control.Category
import Data.Label
import Prelude hiding ((.),id)

-- |

startGUI :: Maybe FilePath -> Maybe Hook -> IO () 
startGUI mfname mhook = do 
  initGUI
  window <- windowNew   

  cfg <- loadConfigFile   
  devlst <- initDevice cfg 
  maxundo <- getMaxUndo cfg >>= 
               \mmax -> maybe (return 50) (return . id) mmax
  (tref,st0,ui,vbox) <- initCoroutine devlst window mfname mhook maxundo  
  setTitleFromFileName st0
  -- writeIORef sref st4
  xinputbool <- getXInputConfig cfg 
  agr <- uiManagerGetActionGroups ui >>= \x ->
           case x of 
             [] -> error "No action group? "
             y:_ -> return y 
  uxinputa <- actionGroupGetAction agr "UXINPUTA" >>= \(Just x) -> 
                return (castToToggleAction x) 
  toggleActionSetActive uxinputa xinputbool
  let canvases = map (getDrawAreaFromBox) . M.elems . getCanvasInfoMap $ st0
  if xinputbool
      then mapM_ (flip widgetSetExtensionEvents [ExtensionEventsAll]) canvases
      else mapM_ (flip widgetSetExtensionEvents [ExtensionEventsNone]) canvases
  

  maybeMenubar <- uiManagerGetWidget ui "/ui/menubar"
  let menubar = case maybeMenubar of 
                  Just x  -> x 
                  Nothing -> error "cannot get menubar from string"
  
  maybeToolbar1 <- uiManagerGetWidget ui "/ui/toolbar1"
  let toolbar1 = case maybeToolbar1 of 
                   Just x  -> x     
                   Nothing -> error "cannot get toolbar from string"
  maybeToolbar2 <- uiManagerGetWidget ui "/ui/toolbar2"
  let toolbar2 = case maybeToolbar2 of 
                   Just x  -> x     
                   Nothing -> error "cannot get toolbar from string" 
  
  containerAdd window vbox
  
  boxPackStart vbox menubar PackNatural 0 
  boxPackStart vbox toolbar1 PackNatural 0
  boxPackStart vbox toolbar2 PackNatural 0 
  boxPackEnd vbox (get rootWindow st0) PackGrow 0 
  -- cursorDot <- cursorNew BlankCursor  
  window `on` deleteEvent $ do
    liftIO $ bouncecallback tref (Menu MenuQuit)
    return True
  putStrLn "before widget show all " 
  widgetShowAll window
  putStrLn "after widget show all " 
  
  -- initialized
  bouncecallback tref Initialized     
  mainGUI 
  return ()
  
