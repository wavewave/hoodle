{-# LANGUAGE ScopedTypeVariables, GADTs, RankNTypes #-}

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

import Control.Lens (view,Simple,Lens,(%~))
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
import Hoodle.Util 
-- 
import Debug.Trace


blockWhile :: (GObjectClass w) => Maybe (ConnectId w) -> IO () -> IO ()
blockWhile msig act = 
  maybe (return ()) signalBlock msig
  >> act 
  >> maybe (return ()) signalUnblock msig
  

-- | reflect view mode UI for current canvas info 
reflectViewModeUI :: MainCoroutine ()
reflectViewModeUI = do 
    xstate <- St.get
    let cinfobox = view currentCanvasInfo xstate 
        ui = view gtkUIManager xstate       
    let mconnid = view (uiComponentSignalHandler.pageModeSignal) xstate
    agr <- liftIO $ uiManagerGetActionGroups ui
    ra1 <- maybe (error "reflectUI") return =<< 
             liftIO (actionGroupGetAction (head agr) "ONEPAGEA")
    let wra1 = castToRadioAction ra1 
    selectBoxAction (pgmodupdate_s mconnid wra1) 
      (pgmodupdate_c mconnid wra1) cinfobox 
    return ()
  where pgmodupdate_s mconnid wra1 _cinfo = do
          liftIO $ blockWhile mconnid $
                     Gtk.set wra1 [radioActionCurrentValue := 1 ] 
        pgmodupdate_c mconnid wra1 _cinfo = do
          liftIO $ blockWhile mconnid $ 
                     Gtk.set wra1 [radioActionCurrentValue := 0 ] 

-- | 
reflectPenModeUI :: MainCoroutine ()
reflectPenModeUI = do 
    reflectUIComponent penModeSignal "PENA" f
  where 
    f xst = Just $
      hoodleModeStateEither (view hoodleModeState xst) #  
        either (\_ -> (penType2Int. Left .view (penInfo.penType)) xst)
               (\_ -> (penType2Int. Right .view (selectInfo.selectType)) xst)


-- | 
reflectPenColorUI :: MainCoroutine () 
reflectPenColorUI = do 
    reflectUIComponent penColorSignal "BLUEA" f
  where 
    f xst = 
      let mcolor = 
            case view (penInfo.penType) xst of 
             PenWork -> Just (view (penInfo.penSet.currPen.penColor) xst)
             HighlighterWork -> Just (view (penInfo.penSet.currHighlighter.penColor) xst)
             _ -> Nothing 
      in fmap color2Int mcolor 
  

-- | 
reflectPenWidthUI :: MainCoroutine () 
reflectPenWidthUI = do 
    reflectUIComponent penPointSignal "PENVERYFINEA" f
  where 
    f xst = 
      case view (penInfo.penType) xst of 
        PenWork -> (Just . point2Int PenWork 
                    . view (penInfo.penSet.currPen.penWidth)) xst
        HighlighterWork -> 
          let x = (Just . point2Int HighlighterWork 
                            . view (penInfo.penSet.currHighlighter.penWidth)) xst
              y = view (penInfo.penSet.currHighlighter.penWidth) xst
          in trace (" x= " ++ show x ++ " y = " ++ show y ) x 
        EraserWork -> (Just . point2Int EraserWork 
                       . view (penInfo.penSet.currEraser.penWidth)) xst
        _ -> Nothing 

-- | 
reflectUIComponent :: Simple Lens UIComponentSignalHandler (Maybe (ConnectId RadioAction))
                   -> String 
                   -> (HoodleState -> Maybe Int)   
                   -> MainCoroutine ()
reflectUIComponent lnz name f = do 
    xst <- St.get 
    let ui = view gtkUIManager xst 
        mconnid = view (uiComponentSignalHandler.lnz) xst 
    agr <- liftIO $ uiManagerGetActionGroups ui 
    Just pma <- liftIO $ actionGroupGetAction (head agr) name 
    let wpma = castToRadioAction pma 
    update xst wpma mconnid   
  where update xst wpma mconnid  = do 
          (f xst) # 
            (maybe (return ()) $ \v -> do
              let action = mkIOaction $ \_evhandler -> do 
                    blockWhile mconnid 
                      (Gtk.set wpma [radioActionCurrentValue := v ] )
                    return (UsrEv ActionOrdered)
              St.modify (tempQueue %~ enqueue action)
              go)
         where go = do r <- nextevent
                       case r of
                         ActionOrdered -> return ()
                         _ -> (liftIO $ print r) >>  go 

