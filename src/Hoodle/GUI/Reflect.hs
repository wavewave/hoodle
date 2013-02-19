{-# LANGUAGE ScopedTypeVariables, GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.GUI.Reflect
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.GUI.Reflect where

import Control.Lens
import Control.Monad
-- import Control.Monad.Loops
import qualified Control.Monad.State as St
import Control.Monad.Trans 
import           Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk as Gtk (set)
--
import Control.Monad.Trans.Crtn.Event
import Control.Monad.Trans.Crtn.Queue
--
import Hoodle.GUI.Menu 
import Hoodle.Type.Canvas
import Hoodle.Type.Coroutine
import Hoodle.Type.Enum 
import Hoodle.Type.HoodleState
import Hoodle.Type.Event

-- | reflect view mode UI for current canvas info 
reflectViewModeUI :: MainCoroutine ()
reflectViewModeUI = do 
    xstate <- St.get
    let cinfobox = view currentCanvasInfo xstate 
        ui = view gtkUIManager xstate       
    let mconnid = view (uiComponentSignalHandler.pageModeSignal) xstate
    liftIO $ maybe (return ()) signalBlock mconnid 
    agr <- liftIO $ uiManagerGetActionGroups ui
    ra1 <- maybe (error "reflectUI") return =<< 
             liftIO (actionGroupGetAction (head agr) "ONEPAGEA")
    let wra1 = castToRadioAction ra1 
    selectBoxAction (pgmodupdate_s wra1) (pgmodupdate_c wra1) cinfobox 
    liftIO $ maybe (return ()) signalUnblock mconnid 
    return ()
  where pgmodupdate_s wra1 _cinfo = do
          liftIO $ Gtk.set wra1 [radioActionCurrentValue := 1 ] 
        pgmodupdate_c wra1 _cinfo = do
          liftIO $ Gtk.set wra1 [radioActionCurrentValue := 0 ] 

-- | 
reflectPenModeUI :: MainCoroutine ()
reflectPenModeUI = do 
    xst <- St.get 
    let ui = view gtkUIManager xst 
        mpenmodconnid = view (uiComponentSignalHandler.penModeSignal) xst 
    agr <- liftIO $ uiManagerGetActionGroups ui 
    Just pma <- liftIO $ actionGroupGetAction (head agr) "PENA"
    let wpma = castToRadioAction pma 
    penmodupdate xst wpma mpenmodconnid   
  where (#) :: a -> (a -> b) -> b 
        (#) = flip ($)
        penmodupdate xst wpma mpenmodconnid  = do 
          let pmodv = hoodleModeStateEither (view hoodleModeState xst) #  
                either (\_ -> (penType2Int. Left .view (penInfo.penType)) xst)
                       (\_ -> (penType2Int. Right .view (selectInfo.selectType)) xst)
          
              action = Left . ActionOrder $ 
                         \evhandler -> do 
                           putStrLn "penmodupdate"
                           putStrLn (show pmodv)
                           -- whileM_ (liftM (>0) eventsPending) (mainIterationDo False)
                           maybe (return ()) signalBlock mpenmodconnid 
                           Gtk.set wpma [radioActionCurrentValue := pmodv ] 
                           maybe (return ()) signalUnblock mpenmodconnid     
                           return ActionOrdered
          St.modify (tempQueue %~ enqueue action)
          go 
         where go = do r <- nextevent
                       case r of
                         ActionOrdered -> (liftIO $ print "ActionOrdered") >> return ()
                         _ -> (liftIO $ print r) >>  go 

