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

import           Control.Lens (view,Simple,Lens)
import qualified Control.Monad.State as St
import           Control.Monad.Trans 
import           Data.Array.MArray
import           Data.Foldable (forM_)
import qualified Data.Map as M (lookup)
import           Data.Word
import           Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk as Gtk (set)
--
-- import Control.Monad.Trans.Crtn.Event
-- import Control.Monad.Trans.Crtn.Queue
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
    unboxBiAct (pgmodupdate_s mconnid wra1) (pgmodupdate_c mconnid wra1) cinfobox 
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
    reflectCursor
  where 
    f xst = Just $
      hoodleModeStateEither (view hoodleModeState xst) #  
        either (\_ -> (penType2Int. Left .view (penInfo.penType)) xst)
               (\_ -> (penType2Int. Right .view (selectInfo.selectType)) xst)


-- | 
reflectPenColorUI :: MainCoroutine () 
reflectPenColorUI = do 
    reflectUIComponent penColorSignal "BLUEA" f
    reflectCursor
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
              doIOaction $ \_evhandler -> do 
                    blockWhile mconnid 
                      (Gtk.set wpma [radioActionCurrentValue := v ] )
                    return (UsrEv ActionOrdered)
              go)
         where go = do r <- nextevent
                       case r of
                         ActionOrdered -> return ()
                         _ -> (liftIO $ print r) >>  go 

-- | 
reflectCursor :: MainCoroutine () 
reflectCursor = do 
    xst <- St.get 
    let useVCursor = view (settings.doesUseVariableCursor) xst 
        cinfobox = view currentCanvasInfo xst 
        canvas = forBoth' unboxBiAct (view drawArea) cinfobox 
        pinfo = view penInfo xst 
        pcolor = view (penSet . currPen . penColor) pinfo
        pwidth = view (penSet . currPen . penWidth) pinfo 
        
    win <- liftIO $ widgetGetDrawWindow canvas
    dpy <- liftIO $ widgetGetDisplay canvas  
    liftIO $ putStrLn ("useVCursor=" ++ show useVCursor); 
    liftIO $ (if useVCursor 
              then do 
                let (r,g,b,a) = case pcolor of  
                                  ColorRGBA r' g' b' a' -> (r',g',b',a')
                                  x -> maybe (0,0,0,1) id (M.lookup pcolor penColorRGBAmap)
                -- cur <- liftIO $ cursorNew Umbrella
                pb <- pixbufNew ColorspaceRgb True 8 10 10 
                pbData <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))
                forM_ [0..99] $ \i -> do 
                  let cvt :: Double -> Word8
                      cvt x | x < 0.0039 = 0
                            | x > 0.996  = 255
                            | otherwise  = fromIntegral (floor (x*256-1) `mod` 256 :: Int)
                  writeArray pbData (4*i)   (cvt r)
                  writeArray pbData (4*i+1) (cvt g)                  
                  writeArray pbData (4*i+2) (cvt b)
                  writeArray pbData (4*i+3) (cvt a)                
                  
                  
                -- putStrLn "width"
                print . length =<<  getElems pbData
                -- pixbuf
                cur <- cursorNewFromPixbuf dpy pb 0 0  
                drawWindowSetCursor win (Just cur)
              else liftIO $ drawWindowSetCursor win Nothing 
             )   
                