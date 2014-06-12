-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.VerticalSpace
-- Copyright   : (c) 2013, 2014 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.VerticalSpace where

import           Control.Applicative
import           Control.Category
import           Control.Lens (view,set,at)
import           Control.Monad hiding (mapM_)
import           Control.Monad.State (get)
import           Control.Monad.Trans (liftIO)
import           Data.Foldable 
import           Data.Monoid
import           Data.Time.Clock
import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.UI.Gtk hiding (get,set) 
-- from hoodle-platform
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple (Dimension(..))
import           Data.Hoodle.Zipper (SeqZipper,toSeq) 
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Type.HitTest
import           Graphics.Hoodle.Render.Type.Hoodle
import           Graphics.Hoodle.Render.Type.Item 
import           Graphics.Hoodle.Render.Util.HitTest
-- from this package
import           Hoodle.Accessor
import           Hoodle.Coroutine.Commit
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Page
import           Hoodle.Coroutine.Pen 
import           Hoodle.Device
import           Hoodle.ModelAction.Page 
import           Hoodle.ModelAction.Select.Transform
import           Hoodle.Type.Alias 
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum 
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.Predefined 
import           Hoodle.View.Coordinate
import           Hoodle.View.Draw
--
import Prelude hiding ((.), id, concat,concatMap,mapM_)

-- | 
splitPageByHLine :: Double 
                 -> Page EditMode 
                 -> ([RItem],Page EditMode,SeqZipper RItemHitted) 
splitPageByHLine y pg = (hitted,set glayers unhitted pg,hltedLayers)
  where 
    alllyrs = view glayers pg
    findHittedItmsInALyr = hltFilteredBy (bboxabove . getBBox) . view gitems
    hltedLayers = fmap findHittedItmsInALyr alllyrs 
    unhitted = fmap (\lyr -> (\x->set gitems x lyr) . concatMap unNotHitted 
                             . getA . findHittedItmsInALyr $ lyr) alllyrs
    hitted = (concat 
              . fmap (concatMap unHitted . getB . findHittedItmsInALyr)
              . toSeq 
              ) alllyrs
    bboxabove (BBox (_,y0) _) = y0 > y 

-- |
verticalSpaceStart :: CanvasId -> PointerCoord -> MainCoroutine () 
verticalSpaceStart cid = commonPenStart verticalSpaceAction cid  
  where 
    verticalSpaceAction _cinfo pnum@(PageNum n) geometry (x,y) = do 
      hdl <- liftM getHoodle get 
      cache <- view renderCache <$> get
      cpg <- getCurrentPageCurr 
      let (itms,npg,hltedLayers) = splitPageByHLine y cpg 
          nhdl = set (gpages.at n) (Just npg) hdl 
          mbbx = (toMaybe . mconcat . fmap (Union . Middle . getBBox)) itms 
      case mbbx of
        Nothing -> return ()   
        Just bbx -> do 
          (sfcbkg,Dim w h) <- liftIO $ canvasImageSurface cache Nothing geometry nhdl 
          sfcitm <- liftIO $ Cairo.createImageSurface 
                               Cairo.FormatARGB32 (floor w) (floor h)
          sfctot <- liftIO $ Cairo.createImageSurface 
                               Cairo.FormatARGB32 (floor w) (floor h)
          liftIO $ Cairo.renderWith sfcitm $ do 
            Cairo.identityMatrix 
            cairoXform4PageCoordinate (mkXform4Page geometry pnum)
            mapM_ (renderRItem cache) itms
          ctime <- liftIO getCurrentTime 
          verticalSpaceProcess cid geometry (bbx,hltedLayers,pnum,cpg) (x,y) 
            (sfcbkg,sfcitm,sfctot) ctime 
          liftIO $ mapM_ Cairo.surfaceFinish [sfcbkg,sfcitm,sfctot]

-- |
addNewPageAndMoveBelow :: (PageNum,SeqZipper RItemHitted,BBox) 
                          -> MainCoroutine () 
addNewPageAndMoveBelow (pnum,hltedLyrs,bbx) = 
    updateXState npgBfrAct 
    >> commit_ 
    >> canvasZoomUpdateAll >> invalidateAll
  where
    npgBfrAct xst = forBoth' unboxBiAct (fsimple xst) . view currentCanvasInfo $ xst
    fsimple :: HoodleState -> CanvasInfo a -> MainCoroutine HoodleState
    fsimple xstate _cinfo = do 
      case view hoodleModeState xstate of 
        ViewAppendState hdl -> do 
          let bsty = view backgroundStyle xstate 
          hdl' <- addNewPageInHoodle bsty PageAfter hdl (unPageNum pnum)
          let hdl'' = moveBelowToNewPage (pnum,hltedLyrs,bbx) hdl' 
              nhdlmodst = ViewAppendState hdl''
          return =<< liftIO . updatePageAll nhdlmodst
                     . set hoodleModeState nhdlmodst $ xstate 
        SelectState _ -> do 
          liftIO $ putStrLn " not implemented yet"
          return xstate
            
-- |             
moveBelowToNewPage :: (PageNum,SeqZipper RItemHitted,BBox) 
                      -> Hoodle EditMode
                      -> Hoodle EditMode
moveBelowToNewPage (PageNum n,hltedLayers,BBox (_,y0) _) hdl = 
    let mpg = view (gpages.at n) hdl 
        mpg2 = view (gpages.at (n+1)) hdl 
    in case (,) <$> mpg <*> mpg2 of    
      Nothing -> hdl 
      Just (pg,pg2) ->  
        let nhlyrs = -- 10 is just a predefined number 
              fmap (fmapAL id 
                     (fmap (changeItemBy (\(x',y')->(x',y'+10-y0)))))
                   hltedLayers
            nlyrs = fmap ((\x -> set gitems x emptyRLayer) 
                          . concatMap unNotHitted 
                          . getA ) nhlyrs 
            npg = set glayers nlyrs pg 
            nnlyrs = fmap ((\x -> set gitems x emptyRLayer)
                          . concatMap unHitted
                          . getB ) nhlyrs
            npg2 = set glayers nnlyrs pg2
            
            nhdl = ( set (gpages.at (n+1)) (Just npg2)
                   . set (gpages.at n) (Just npg) ) hdl 
        in nhdl 


-- |
verticalSpaceProcess :: CanvasId
                     -> CanvasGeometry
                     -> (BBox,SeqZipper RItemHitted,PageNum,Page EditMode)
                     -> (Double,Double)
                     -> (Cairo.Surface,Cairo.Surface,Cairo.Surface) 
                          -- ^ (background, item, total) 
                     -> UTCTime
                     -> MainCoroutine () 
verticalSpaceProcess cid geometry pinfo@(bbx,hltedLayers,pnum@(PageNum n),pg) 
                     (x0,y0) sfcs@(sfcbkg,sfcitm,sfctot) otime = do 
    r <- nextevent 
    xst <- get
    forBoth' unboxBiAct (f r xst) . getCanvasInfo cid $ xst 
  where 
    Dim w h = view gdimension pg    
    CvsCoord (_,y0_cvs) = (desktop2Canvas geometry . page2Desktop geometry) (pnum,PageCoord (x0,y0))
    -------------------------------------------------------------
    f :: UserEvent -> HoodleState -> CanvasInfo a -> MainCoroutine ()
    f r xstate cvsInfo = penMoveAndUpOnly r pnum geometry defact 
                           (moveact xstate cvsInfo) upact
    -------------------------------------------------------------  
    defact = verticalSpaceProcess cid geometry pinfo (x0,y0) sfcs otime
    -------------------------------------------------------------    
    upact pcoord = do 
      let mpgcoord = (desktop2Page geometry . device2Desktop geometry) pcoord
      case mpgcoord of 
        Nothing -> invalidateAll 
        Just (cpn,PageCoord (_,y)) -> 
          if cpn /= pnum 
          then invalidateAll 
          else do 
            -- add space  within this page
            let BBox _ (_,by1) = bbx
            if by1 + y - y0 < h 
              then do 
                xst <- get 
                let hdl = getHoodle xst 
                let nhlyrs = 
                      fmap (fmapAL id 
                            (fmap (changeItemBy (\(x',y')->(x',y'+y-y0)))))
                                  hltedLayers
                    nlyrs = fmap 
                              ((\is -> set gitems is emptyRLayer) 
                                      . concat
                                      . interleave unNotHitted unHitted) 
                              nhlyrs 
                    npg = set glayers nlyrs pg 
                    nhdl = set (gpages.at n) (Just npg) hdl 
                commit (set hoodleModeState (ViewAppendState nhdl) xst)
                invalidateAll 
              else do 
                addNewPageAndMoveBelow (pnum,hltedLayers,bbx)
    -------------------------------------------------------------
    moveact _xstate cvsInfo (_,(x,y)) = 
      processWithDefTimeInterval 
        (verticalSpaceProcess cid geometry pinfo (x0,y0) sfcs)
        (\ctime -> do 
           let CvsCoord (_,y_cvs) = 
                 (desktop2Canvas geometry . page2Desktop geometry) (pnum,PageCoord (x,y))
               BBox _ (_,by1) = bbx                 
               mode | by1 + y - y0 > h = OverPage
                    | y > y0 = GoingDown
                    | otherwise = GoingUp 
               z = canvas2DesktopRatio geometry 
               drawguide = do 
                 Cairo.identityMatrix 
                 cairoXform4PageCoordinate (mkXform4Page geometry pnum )
                 Cairo.setLineWidth (predefinedLassoWidth*z)
                 case mode of
                   GoingUp   -> Cairo.setSourceRGBA 0.1 0.8 0.1 0.4
                   GoingDown -> Cairo.setSourceRGBA 0.1 0.1 0.8 0.4 
                   OverPage  -> Cairo.setSourceRGBA 0.8 0.1 0.1 0.4
                 Cairo.moveTo 0 y0
                 Cairo.lineTo w y0 
                 Cairo.stroke 
                 Cairo.moveTo 0 y
                 Cairo.lineTo w y
                 Cairo.stroke  
                 case mode of
                   GoingUp   -> Cairo.setSourceRGBA 0.1 0.8 0.1 0.2 >>
                                Cairo.rectangle 0 y w (y0-y)                    
                   GoingDown -> Cairo.setSourceRGBA 0.1 0.1 0.8 0.2 >>
                                Cairo.rectangle 0 y0 w (y-y0)
                   OverPage  -> Cairo.setSourceRGBA 0.8 0.1 0.1 0.2 >> 
                                Cairo.rectangle 0 y0 w (y-y0)
                 Cairo.fill
           liftIO $ Cairo.renderWith sfctot $ do 
             Cairo.setSourceSurface sfcbkg 0 0
             Cairo.setOperator Cairo.OperatorSource
             Cairo.paint 
             Cairo.setSourceSurface sfcitm 0 (y_cvs-y0_cvs)
             Cairo.setOperator Cairo.OperatorOver
             Cairo.paint
             drawguide 
           let canvas = view drawArea cvsInfo 
           win <- liftIO $ widgetGetDrawWindow canvas
           liftIO $ renderWithDrawable win $ do 
             Cairo.setSourceSurface sfctot 0 0 
             Cairo.setOperator Cairo.OperatorSource 
             Cairo.paint 
           verticalSpaceProcess cid geometry pinfo (x0,y0) sfcs ctime)
        otime 

        
    
