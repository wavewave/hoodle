-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.VerticalSpace
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.VerticalSpace where

import Control.Category
-- import Data.Label
import qualified Data.IntMap as IM
import Control.Lens
import Control.Monad hiding (mapM_)
import Control.Monad.State (get)
-- import qualified Control.Monad.State as St
import Data.Foldable 
import           Data.Monoid
import Data.Time.Clock
import           Graphics.Rendering.Cairo 
import           Graphics.UI.Gtk hiding (get,set) 
-- 
import Data.Hoodle.Generic
import Data.Hoodle.BBox
import           Data.Hoodle.Simple (Dimension(..))
import           Data.Hoodle.Zipper (toSeq,current,SeqZipper(..))
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Generic
import           Graphics.Hoodle.Render.Type.HitTest
import           Graphics.Hoodle.Render.Type.Hoodle
import           Graphics.Hoodle.Render.Type.Item 
import           Graphics.Hoodle.Render.Util.HitTest
-- 
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Page
import           Hoodle.Coroutine.Scroll
import           Hoodle.Coroutine.Select
import Hoodle.Coroutine.Commit
import Hoodle.Coroutine.Pen 
import           Hoodle.Device
import           Hoodle.ModelAction.Page 
import           Hoodle.ModelAction.Select.Transform
import Hoodle.Type.Alias 
import Hoodle.Type.Event
import Hoodle.Type.Coroutine
import Hoodle.Type.Canvas
import Hoodle.Type.HoodleState
import Hoodle.Type.PageArrangement
import Hoodle.Type.Predefined 
import Hoodle.Device
import Hoodle.View.Coordinate
import Hoodle.View.Draw

import Hoodle.Accessor
import Hoodle.ModelAction.Page
import Hoodle.ModelAction.Eraser
import Hoodle.ModelAction.Layer
import Hoodle.Util
--
import Prelude hiding ((.), id, concat,concatMap,mapM_)

-- | 
splitPageByHLine :: Double -> Page EditMode 
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
    bboxabove (BBox (x0,y0) (x1,y1)) = y0 > y 


-- |
verticalSpaceStart :: CanvasId -> PointerCoord -> MainCoroutine () 
verticalSpaceStart cid = commonPenStart verticalSpaceAction cid  
  where 
    verticalSpaceAction _cinfo pnum@(PageNum n) geometry (x,y) = do 
      hdl <- liftM getHoodle get 
      cpg <- getCurrentPageCurr 
      let (itms,npg,hltedLayers) = splitPageByHLine y cpg 
          nhdl = set (gpages.at n) (Just npg) hdl 
          mbbx = (toMaybe . mconcat . fmap (Union . Middle . getBBox)) itms 

      -- liftIO $ print bbx 
      case mbbx of
        Nothing -> return ()   
        Just bbx -> do 
          liftIO $ print bbx
          (sfcbkg,Dim w h) <- liftIO $ canvasImageSurface Nothing geometry nhdl 
          sfcitm <- liftIO $ createImageSurface FormatARGB32 (floor w) (floor h)
          sfctot <- liftIO $ createImageSurface FormatARGB32 (floor w) (floor h)
          liftIO $ renderWith sfcitm $ do 
            identityMatrix 
            cairoXform4PageCoordinate geometry pnum
            mapM_ renderRItem itms
          ctime <- liftIO getCurrentTime 
          
          -- liftIO (splitPageByHLine y cpg )
          verticalSpaceProcess cid geometry (bbx,hltedLayers,pnum,cpg) (x,y) 
            (sfcbkg,sfcitm,sfctot) ctime 

          liftIO $ mapM_ surfaceFinish [sfcbkg,sfcitm,sfctot]

          
-- |

verticalSpaceProcess :: CanvasId
                     -> CanvasGeometry
                     -> (BBox,SeqZipper RItemHitted,PageNum,Page EditMode)
                     -> (Double,Double)
                     -> (Surface,Surface,Surface)
                     -> UTCTime
                     -> MainCoroutine () 
verticalSpaceProcess cid geometry pinfo@(bbx,hltedLayers,pnum@(PageNum n),pg) 
                     (x0,y0) sfcs@(sfcbkg,sfcitm,sfctot) otime = do 
    r <- nextevent 
    xst <- get
    boxAction (f r xst) . getCanvasInfo cid $ xst 
  where 
    Dim w h = view gdimension pg    
    CvsCoord (x0_cvs,y0_cvs) = 
      (desktop2Canvas geometry . page2Desktop geometry) (pnum,PageCoord (x0,y0))
    
    f :: (ViewMode a) => MyEvent -> HoodleState -> CanvasInfo a -> MainCoroutine ()
    f r xstate cvsInfo = penMoveAndUpOnly r pnum geometry defact 
                           (moveact xstate cvsInfo) upact
                           
    defact = verticalSpaceProcess cid geometry pinfo (x0,y0) sfcs otime
    upact pcoord = do 
      let mpgcoord = (desktop2Page geometry . device2Desktop geometry) pcoord
      case mpgcoord of 
        Nothing -> invalidateAll 
        Just (cpn,PageCoord (x,y)) -> 
          if cpn /= pnum 
          then invalidateAll 
          else do 
            let BBox (bx0,by0) (bx1,by1) = bbx
            if by1 + y - y0 < h 
              then do 
                xst <- get 
                let hdl = getHoodle xst 
                let nhlyrs = 
                      fmap (fmapAL id 
                            (fmap (changeItemBy (\(x',y')->(x',y'+y-y0)))))
                                  hltedLayers
                    nlyrs = fmap 
                              ((\x -> set gitems x emptyRLayer) 
                                      . concat
                                      . interleave unNotHitted unHitted) 
                              nhlyrs 
                    npg = set glayers nlyrs pg 
                    nhdl = set (gpages.at n) (Just npg) hdl 
                commit (set hoodleModeState (ViewAppendState nhdl) xst)
                invalidateAll 
              else invalidateAll   
                
    moveact xstate cvsInfo (pcoord,(x,y)) = 
      processWithDefTimeInterval 
        (verticalSpaceProcess cid geometry pinfo (x0,y0) sfcs)
        (\ctime -> do 
           let CvsCoord (x_cvs,y_cvs) = 
                 (desktop2Canvas geometry . page2Desktop geometry) (pnum,PageCoord (x,y))
                 
               drawguide = do 
                 identityMatrix 
                 cairoXform4PageCoordinate geometry pnum 
                 setSourceRGBA 0 0 0 1 
                 moveTo 0 y0
                 lineTo w y0 
                 stroke 
                 moveTo 0 y
                 lineTo w y
                 stroke 
           liftIO $ renderWith sfctot $ do 
             setSourceSurface sfcbkg 0 0
             setOperator OperatorSource
             paint 
             setSourceSurface sfcitm 0 (y_cvs-y0_cvs)
             setOperator OperatorOver
             paint
             drawguide
           let canvas = view drawArea cvsInfo 
           win <- liftIO $ widgetGetDrawWindow canvas
           liftIO $ renderWithDrawable win $ do 
             setSourceSurface sfctot 0 0 
             setOperator OperatorSource 
             paint 

           verticalSpaceProcess cid geometry pinfo (x0,y0) sfcs ctime)
        otime 

        
    
{-    

    f r xstate cvsInfo = penMoveAndUpOnly r pnum geometry defact 
                                 (moveact xstate cvsInfo) upact
    defact = verticalSpaceProcess cid pnum geometry itms (x0,y0)
    upact _ = invalidateAll
    moveact xstate cvsInfo (_pcoord,(x,y)) = do 
      let line = ((x0,y0),(x,y))
          hittestbbox = hltHittedByLineRough line itms
          (hittestitem,hitState) = 
            St.runState (hltItmsHittedByLineFrmSelected_StateT line hittestbbox) False
      if hitState 
        then do 
          page <- getCurrentPageCvsId cid 
          let currhdl     = unView . view hoodleModeState $ xstate 
              dim         = view gdimension page
              pgnum       = view currentPageNum cvsInfo
              currlayer = getCurrentLayer page
          let (newitms,maybebbox1) = St.runState (eraseHitted hittestitem) Nothing
              maybebbox = fmap (flip inflate 2.0) maybebbox1
          newlayerbbox <- liftIO . updateLayerBuf dim maybebbox 
                          . set gitems newitms $ currlayer 
          let newpagebbox = adjustCurrentLayer newlayerbbox page 
              newhdlbbox = over gpages (IM.adjust (const newpagebbox) pgnum) currhdl
              newhdlmodst = ViewAppendState newhdlbbox
          commit . set hoodleModeState newhdlmodst 
            =<< (liftIO (updatePageAll newhdlmodst xstate))
          invalidateInBBox Nothing Efficient cid 
          nitms <- rItmsInCurrLyr
          verticalSpaceProcess cid pnum geometry nitms (x,y)
        else verticalSpaceProcess cid pnum geometry itms (x,y) 
  -}          
