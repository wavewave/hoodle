{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.GUI.Reflect
-- Copyright   : (c) 2013, 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.GUI.Reflect where

import           Control.Lens (view, Simple,Lens, (^.))
import           Control.Monad.State as St
import           Data.Array.MArray
import qualified Data.Foldable as F (forM_,mapM_) 
import qualified Data.Map as M (lookup)
import           Data.Word
import qualified Graphics.UI.Gtk as Gtk
--
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.GUI.Menu 
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum 
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.Predefined 
import           Hoodle.Util 
import           Hoodle.View.Coordinate
-- 
-- import Debug.Trace

-- | 
changeCurrentCanvasId :: CanvasId -> MainCoroutine UnitHoodle
changeCurrentCanvasId cid = do 
    xst <- St.get
    let uhdl = view (unitHoodles.currentUnit) xst
    case setCurrentCanvasId cid uhdl of
      Nothing -> return uhdl
      Just uhdl' -> do 
        pureUpdateUhdl (const uhdl')
        reflectViewModeUI
        return uhdl'


-- | check current canvas id and new active canvas id and invalidate if it's 
--   changed. 
chkCvsIdNInvalidate :: CanvasId -> MainCoroutine () 
chkCvsIdNInvalidate cid = do 
  currcid <- liftM (getCurrentCanvasId . view (unitHoodles.currentUnit) ) St.get 
  when (currcid /= cid) (changeCurrentCanvasId cid >> invalidateAll)

-- | block signal for act
blockWhile :: (Gtk.GObjectClass w) => Maybe (Gtk.ConnectId w) -> IO () -> IO ()
blockWhile msig act = do
    F.mapM_ (\_ -> print "signal will be blocked") msig
    F.mapM_ Gtk.signalBlock msig >> act >> F.mapM_ Gtk.signalUnblock msig


-- | reflect view mode UI for current canvas info 
reflectViewModeUI :: MainCoroutine ()
reflectViewModeUI = do 
    xstate <- St.get
    let uhdl = view (unitHoodles.currentUnit) xstate
        cinfobox = view currentCanvasInfo uhdl
        ui = view gtkUIManager xstate
    let mconnid = view (uiComponentSignalHandler.pageModeSignal) xstate
    agr <- liftIO $ Gtk.uiManagerGetActionGroups ui
    ra1 <- maybe (error "reflectUI") return =<< 
             liftIO (Gtk.actionGroupGetAction (head agr) "ONEPAGEA")
    let wra1 = Gtk.castToRadioAction ra1 
    unboxBiAct (pgmodupdate_s mconnid wra1) (pgmodupdate_c mconnid wra1) cinfobox 
    return ()
  where pgmodupdate_s mconnid wra1 _cinfo = do
          liftIO $ blockWhile mconnid $
                     Gtk.set wra1 [Gtk.radioActionCurrentValue Gtk.:= 1 ] 
        pgmodupdate_c mconnid wra1 _cinfo = do
          liftIO $ blockWhile mconnid $ 
                     Gtk.set wra1 [Gtk.radioActionCurrentValue Gtk.:= 0 ] 

-- | 
reflectPenModeUI :: MainCoroutine ()
reflectPenModeUI = do 
    reflectUIRadio penModeSignal "PENA" f
    reflectCursor
  where 
    f xst = Just $
      hoodleModeStateEither ((view hoodleModeState . view (unitHoodles.currentUnit)) xst) #  
        either (\_ -> (penType2Int. Left .view (penInfo.penType)) xst)
               (\_ -> (penType2Int. Right .view (selectInfo.selectType)) xst)


-- | 
reflectPenColorUI :: MainCoroutine () 
reflectPenColorUI = do 
    reflectUIRadio penColorSignal "BLUEA" f
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
    reflectUIRadio penPointSignal "PENVERYFINEA" f
    reflectCursor
  where 
    f xst = 
      case view (penInfo.penType) xst of 
        PenWork -> (Just . point2Int PenWork 
                    . view (penInfo.penSet.currPen.penWidth)) xst
        HighlighterWork -> 
          let x = (Just . point2Int HighlighterWork 
                            . view (penInfo.penSet.currHighlighter.penWidth)) xst
              -- y = view (penInfo.penSet.currHighlighter.penWidth) xst
          in x 
        EraserWork -> (Just . point2Int EraserWork 
                       . view (penInfo.penSet.currEraser.penWidth)) xst
        _ -> Nothing 

-- |
reflectNewPageModeUI :: MainCoroutine ()
reflectNewPageModeUI = 
    reflectUIRadio newPageModeSignal "NEWPAGEPLAINA" (Just . newPageMode2Int . (^. settings.newPageMode))

-- | 
reflectUIRadio :: Simple Lens UIComponentSignalHandler (Maybe (Gtk.ConnectId Gtk.RadioAction))
               -> String 
               -> (HoodleState -> Maybe Int)   
               -> MainCoroutine ()
reflectUIRadio lnz name f = do 
    xst <- St.get 
    let ui = view gtkUIManager xst 
        mconnid = view (uiComponentSignalHandler.lnz) xst 
    agr <- liftIO $ Gtk.uiManagerGetActionGroups ui 
    Just pma <- liftIO $ Gtk.actionGroupGetAction (head agr) name 
    let wpma = Gtk.castToRadioAction pma 
    update xst wpma mconnid   
  where update xst wpma mconnid  = do 
          (f xst) # 
            (maybe (return ()) $ \v ->
              doIOaction_ $ blockWhile mconnid (Gtk.set wpma [Gtk.radioActionCurrentValue Gtk.:= v ] )
            )


-- | this function must be moved to GUI.Reflect
reflectUIToggle :: Gtk.UIManager -> String -> Bool -> IO ()
reflectUIToggle ui str b = do 
    agr <- Gtk.uiManagerGetActionGroups ui >>= \x -> 
      case x of
        [] -> error "No action group?"
        y:_ -> return y
    Just savea <- Gtk.actionGroupGetAction agr str -- ("SAVEA" :: String)
    Gtk.actionSetSensitive savea b

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
          let uhdl       = view (unitHoodles.currentUnit) xst
              cinfobox   = view currentCanvasInfo uhdl           
              canvas     = forBoth' unboxBiAct (view drawArea) cinfobox           
          win <- Gtk.widgetGetDrawWindow canvas
          Gtk.postGUIAsync (Gtk.drawWindowSetCursor win Nothing) 
          return (UsrEv ActionOrdered)
        go 
 where 
   act xst = doIOaction $ \_ -> do 
     Gtk.postGUIAsync $ do 
       let uhdl = view (unitHoodles.currentUnit) xst
           -- mcur       = view cursorInfo xst 
           cinfobox   = view currentCanvasInfo uhdl
           canvas     = forBoth' unboxBiAct (view drawArea) cinfobox 
           cpn        = PageNum $ 
                          forBoth' unboxBiAct (view currentPageNum) cinfobox
           pinfo = view penInfo xst 
           pcolor = view (penSet . currPen . penColor) pinfo
           pwidth = view (penSet . currPen . penWidth) pinfo 
       win <- Gtk.widgetGetDrawWindow canvas
       dpy <- Gtk.widgetGetDisplay canvas  

       geometry <- 
         forBoth' unboxBiAct (\c -> let arr = view (viewInfo.pageArrangement) c
                                    in makeCanvasGeometry cpn arr canvas
                             ) cinfobox
       let p2c = desktop2Canvas geometry . page2Desktop geometry
           CvsCoord (x0,_y0) = p2c (cpn, PageCoord (0,0))  
           CvsCoord (x1,_y1) = p2c (cpn, PageCoord (pwidth,pwidth))
           cursize = (x1-x0) 
           (r,g,b,a) = case pcolor of  
                         ColorRGBA r' g' b' a' -> (r',g',b',a')
                         _ -> maybe (0,0,0,1) id (M.lookup pcolor penColorRGBAmap)
       pb <- Gtk.pixbufNew Gtk.ColorspaceRgb True 8 maxCursorWidth maxCursorHeight 
       let numPixels = maxCursorWidth*maxCursorHeight
       pbData <- (Gtk.pixbufGetPixels pb :: IO (Gtk.PixbufData Int Word8))
       F.forM_ [0..numPixels-1] $ \i -> do 
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

       Gtk.drawWindowSetCursor win . Just =<< 
         Gtk.cursorNewFromPixbuf dpy pb (floor cursize `div` 2) (floor cursize `div` 2)
     return (UsrEv ActionOrdered)
