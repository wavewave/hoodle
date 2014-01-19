{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.GUI.Reflect
-- Copyright   : (c) 2013-2014 Ian-Woo Kim
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
--
import Hoodle.GUI.Menu 
import Hoodle.Type.Canvas
import Hoodle.Type.Coroutine
import Hoodle.Type.Enum 
import Hoodle.Type.Event
import Hoodle.Type.HoodleState
import Hoodle.Type.PageArrangement
import Hoodle.Type.Predefined 
import Hoodle.Util 
import Hoodle.View.Coordinate
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
    reflectCursor
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
                         _ -> go 

-- | 
reflectCursor :: MainCoroutine () 
reflectCursor = do 
    xst <- St.get 
    let useVCursor = view (settings.doesUseVariableCursor) xst 
    let go = do r <- nextevent 
                case r of
                  ActionOrdered -> return ()
                  _ -> go 
    if useVCursor 
      then 
        act xst >> go 
      else do 
        doIOaction $ \_ -> do
          let cinfobox   = view currentCanvasInfo xst           
              canvas     = forBoth' unboxBiAct (view drawArea) cinfobox
#ifdef GTK3
          Just win <- widgetGetWindow canvas
#else // GTK3 
          win <- widgetGetDrawWindow canvas
#endif // GTK3
          postGUIAsync (drawWindowSetCursor win Nothing) 
          return (UsrEv ActionOrdered)
        go 
 where act xst = doIOaction $ \_ -> do 
         let -- mcur       = view cursorInfo xst 
             cinfobox   = view currentCanvasInfo xst 
             canvas     = forBoth' unboxBiAct (view drawArea) cinfobox 
             cpn        = PageNum $ 
                            forBoth' unboxBiAct (view currentPageNum) cinfobox
             pinfo = view penInfo xst 
             pcolor = view (penSet . currPen . penColor) pinfo
             pwidth = view (penSet . currPen . penWidth) pinfo 
#ifdef GTK3
         Just win <- widgetGetWindow canvas
#else // GTK3
         win <- widgetGetDrawWindow canvas
#endif // GTK3
         dpy <- widgetGetDisplay canvas  
         
         geometry <- 
           forBoth' unboxBiAct (\c -> let arr = view (viewInfo.pageArrangement) c
                                      in makeCanvasGeometry cpn arr canvas
                               ) cinfobox
         let p2c = desktop2Canvas geometry . page2Desktop geometry
             CvsCoord (x0,y0) = p2c (cpn, PageCoord (0,0))  
             CvsCoord (x1,y1) = p2c (cpn, PageCoord (pwidth,pwidth))
             cursize = (x1-x0) 
             (r,g,b,a) = case pcolor of  
                           ColorRGBA r' g' b' a' -> (r',g',b',a')
                           x -> maybe (0,0,0,1) id (M.lookup pcolor penColorRGBAmap)
         pb <- pixbufNew ColorspaceRgb True 8 maxCursorWidth maxCursorHeight 
         let numPixels = maxCursorWidth*maxCursorHeight
         pbData <- (pixbufGetPixels pb :: IO (PixbufData Int Word8))
         forM_ [0..numPixels-1] $ \i -> do 
           let cvt :: Double -> Word8
               cvt x | x < 0.0039 = 0
                     | x > 0.996  = 255
                     | otherwise  = fromIntegral (floor (x*256-1) `mod` 256 :: Int)
           if (fromIntegral (i `mod` maxCursorWidth)) < cursize 
              && (fromIntegral (i `div` maxCursorWidth)) < cursize 
             then do 
               writeArray pbData (4*i)   (cvt r)
               writeArray pbData (4*i+1) (cvt g)                  
               writeArray pbData (4*i+2) (cvt b)
               writeArray pbData (4*i+3) (cvt a)
             else do
               writeArray pbData (4*i)   0
               writeArray pbData (4*i+1) 0
               writeArray pbData (4*i+2) 0
               writeArray pbData (4*i+3) 0
            
         postGUIAsync . drawWindowSetCursor win . Just =<< 
           cursorNewFromPixbuf dpy pb 
             (floor cursize `div` 2) (floor cursize `div` 2)
         return (UsrEv ActionOrdered)
