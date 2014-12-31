{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Page 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Page where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens (view,set,(.~), (^.))
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Reader (ask)
import qualified Data.Foldable as F
import           Data.Function (on)
import qualified Data.IntMap as M
import           Data.List (sortBy)
import           Data.UUID.V4
import qualified Graphics.Rendering.Cairo as Cairo
-- from hoodle-platform
import           Data.Hoodle.Generic
import           Data.Hoodle.Select
import           Data.Hoodle.Simple (Dimension(..))
import           Data.Hoodle.Zipper
import           Graphics.Hoodle.Render
-- import           Graphics.Hoodle.Render.Background
import           Graphics.Hoodle.Render.Type
-- from this package
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Commit
import           Hoodle.Coroutine.Scroll
import           Hoodle.ModelAction.Page
import           Hoodle.Type.Alias
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Canvas
import           Hoodle.Type.Event
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.HoodleState
import           Hoodle.Type.Enum
import           Hoodle.Util
import           Hoodle.View.Coordinate
-- 

-- | change page of current canvas using a modify function
changePage :: (Int -> Int) -> MainCoroutine () 
changePage modifyfn = (view backgroundStyle <$> get) >>= \bsty -> 
                         updateUhdl (changePageAction bsty) >>
                         adjustScrollbarWithGeometryCurrent >>
                         invalidateAllInBBox Nothing Efficient  
  where changePageAction bsty uhdl = unboxBiAct (fsingle bsty uhdl) (fcont bsty uhdl) 
                               . (^. currentCanvasInfo) $ uhdl
        fsingle bsty uhdl cvsInfo = do 
          let xojst = view hoodleModeState $ uhdl
              npgnum = modifyfn (cvsInfo ^. currentPageNum)
              cid = view canvasId cvsInfo
          (b,npgnum',_,xojst') <- changePageInHoodleModeState bsty npgnum xojst
          -- xstate1 <- get
          uhdl' <- liftIO $ updatePageAll xojst' uhdl
          ncvsInfo <- liftIO $ setPage uhdl' (PageNum npgnum') cid
          let uhdlfinal = (currentCanvasInfo .~ ncvsInfo) uhdl'
          when b $ updateUhdl (const (return uhdlfinal)) >> commit_
          return uhdlfinal 
        
        fcont bsty uhdl cvsInfo = do 
          let xojst = view hoodleModeState uhdl
              npgnum = modifyfn (cvsInfo ^. currentPageNum)
              cid  = cvsInfo ^. canvasId
          (b,npgnum',_,xojst') <- changePageInHoodleModeState bsty npgnum xojst
          uhdl' <- liftIO $ updatePageAll xojst' uhdl
          ncvsInfo <- liftIO $ setPage uhdl' (PageNum npgnum') cid
          let uhdlfinal = (currentCanvasInfo .~ ncvsInfo) uhdl'
          when b $ updateUhdl (const (return uhdlfinal)) >> commit_
          return uhdlfinal 


-- | 
changePageInHoodleModeState :: BackgroundStyle 
                            -> Int  -- ^ new page number 
                            -> HoodleModeState 
                            -> MainCoroutine (Bool,Int,Page EditMode,HoodleModeState)
changePageInHoodleModeState bsty npgnum hdlmodst = do
    let ehdl = hoodleModeStateEither hdlmodst
        pgs = either (view gpages) (view gselAll) ehdl
        totnumpages = M.size pgs
        lpage = maybeError' "changePage" (M.lookup (totnumpages-1) pgs)
    (isChanged,npgnum',npage',ehdl') <- 
      if (npgnum >= totnumpages) 
        then do 
          let cbkg = view gbackground lpage
          nbkg <- newBkg bsty cbkg  
          npage <- set gbackground nbkg <$> (newPageFromOld lpage)
          geometry <- liftIO . getGeometry4CurrCvs . view (unitHoodles.currentUnit) =<< get
          callRenderer_ $ updateBkgCache geometry (PageNum (totnumpages-1),npage)
          let npages = M.insert totnumpages npage pgs  
          return  (True,totnumpages,npage,
                     either (Left . set gpages npages) (Right. set gselAll npages) ehdl ) 
        else do
          let npg = if npgnum < 0 then 0 else npgnum
              pg = maybeError' "changePage" (M.lookup npg pgs)
          return (False,npg,pg,ehdl) 
    return (isChanged,npgnum',npage',either ViewAppendState SelectState ehdl')


-- | 
canvasZoomUpdateGenRenderCvsId :: MainCoroutine () 
                                  -> CanvasId 
                                  -> Maybe ZoomMode 
                                  -> Maybe (PageNum,PageCoordinate) 
                                  -> MainCoroutine ()
canvasZoomUpdateGenRenderCvsId renderfunc cid mzmode mcoord = do 
    updateUhdl zoomUpdateAction 
    adjustScrollbarWithGeometryCvsId cid
    xst <- get
    let uhdl = view (unitHoodles.currentUnit) xst
        hdl = getHoodle uhdl
    geometry <- liftIO (getGeometry4CurrCvs uhdl)
    let cpn = view (unboxLens currentPageNum) .  getCanvasInfo cid $ uhdl
    let plst = sortBy ( compare `on` (\(n,_) -> abs (n - cpn)) ) . zip [0..] . F.toList $ hdl ^. gpages
    forM_ plst $ \(pn,pg) -> callRenderer_ (updateBkgCache geometry (PageNum pn,pg))
    renderfunc
  where zoomUpdateAction uhdl =  
          unboxBiAct (fsingle uhdl) (fcont uhdl) . getCanvasInfo cid $ uhdl 
        fsingle uhdl cinfo = do   
          geometry <- liftIO $ getCvsGeomFrmCvsInfo cinfo 
          page <- getCurrentPageCvsId cid
          let zmode = maybe (cinfo ^. viewInfo.zoomMode) id mzmode  
              pdim = PageDimension $ page ^. gdimension
              xy = either (const (0,0)) (unPageCoord.snd) 
                     (getCvsOriginInPage geometry)
              cdim = canvasDim geometry 
              narr = makeSingleArrangement zmode pdim cdim xy  
              ncinfobox = CanvasSinglePage
                          . (viewInfo.pageArrangement .~ narr)
                          . (viewInfo.zoomMode .~  zmode) $ cinfo
          return . modifyCanvasInfo cid (const ncinfobox) $ uhdl
        fcont uhdl cinfo = do   
          geometry <- liftIO $ getCvsGeomFrmCvsInfo cinfo 
          let zmode = maybe (view (viewInfo.zoomMode) cinfo) id mzmode 
              cpn = PageNum $ view currentPageNum cinfo 
              cdim = canvasDim geometry 
              hdl = getHoodle uhdl
              origcoord = case mcoord of
                            Just coord -> coord 
                            Nothing -> either (const (cpn,PageCoord (0,0))) id 
                                         (getCvsOriginInPage geometry)
              narr = makeContinuousArrangement zmode cdim hdl origcoord
              ncinfobox = CanvasContPage
                          . (viewInfo.pageArrangement .~  narr)
                          . (viewInfo.zoomMode .~ zmode) $ cinfo
          return . modifyCanvasInfo cid (const ncinfobox) $ uhdl

-- | 
canvasZoomUpdateCvsId :: CanvasId 
                         -> Maybe ZoomMode 
                         -> MainCoroutine ()
canvasZoomUpdateCvsId cid mzmode = 
  canvasZoomUpdateGenRenderCvsId invalidateAll cid mzmode Nothing
  
-- | 
canvasZoomUpdateBufAll :: MainCoroutine () 
canvasZoomUpdateBufAll = do 
    klst <- M.keys . view cvsInfoMap . view (unitHoodles.currentUnit) <$> get
    mapM_ updatefunc klst 
  where 
    updatefunc cid 
      = canvasZoomUpdateGenRenderCvsId  (invalidateInBBox Nothing Efficient cid) cid Nothing Nothing 


-- |
canvasZoomUpdateAll :: MainCoroutine () 
canvasZoomUpdateAll = do 
  klst <- M.keys . view cvsInfoMap . view (unitHoodles.currentUnit) <$> get
  mapM_ (flip canvasZoomUpdateCvsId Nothing) klst 


-- | 
canvasZoomUpdate :: Maybe ZoomMode -> MainCoroutine () 
canvasZoomUpdate mzmode = do  
  cid <- getCurrentCanvasId . view (unitHoodles.currentUnit) <$> get
  canvasZoomUpdateCvsId cid mzmode

-- |
pageZoomChange :: ZoomMode -> MainCoroutine () 
pageZoomChange = canvasZoomUpdate . Just 

-- | 
pageZoomChangeRel :: ZoomModeRel -> MainCoroutine () 
pageZoomChangeRel rzmode = do 
    forBoth' unboxBiAct fsingle . view currentCanvasInfo . view (unitHoodles.currentUnit) =<< get 
  where 
    fsingle :: CanvasInfo a -> MainCoroutine ()
    fsingle cinfo = do 
      let cpn    = PageNum (cinfo ^. currentPageNum)
          arr    = cinfo ^. viewInfo.pageArrangement 
          canvas = cinfo ^. drawArea 
      geometry <- liftIO $ makeCanvasGeometry cpn arr canvas
      let nratio = relZoomRatio geometry rzmode
      pageZoomChange (Zoom nratio)

-- |
newPage :: AddDirection -> MainCoroutine () 
newPage dir = (view backgroundStyle <$> get) >>= \bsty ->
                updateUhdl (npgBfrAct bsty) >>
                commit_                     >>
                canvasZoomUpdateAll         >>
                invalidateAll
  where 
    npgBfrAct bsty uhdl = forBoth' unboxBiAct (fsimple bsty uhdl) . view currentCanvasInfo $ uhdl
    fsimple :: BackgroundStyle -> UnitHoodle -> CanvasInfo a -> MainCoroutine UnitHoodle
    fsimple bsty uhdl cinfo = do 
      case view hoodleModeState uhdl of 
        ViewAppendState hdl -> do 
          hdl' <- addNewPageInHoodle bsty dir hdl (view currentPageNum cinfo)
          liftIO . updatePageAll (ViewAppendState hdl')
                 . set hoodleModeState  (ViewAppendState hdl') $ uhdl
        SelectState _ -> do 
          msgShout  "newPage: not implemented yet"
          return uhdl
      
-- | delete current page of current canvas
deleteCurrentPage :: MainCoroutine ()           
deleteCurrentPage = 
    updateUhdl delpgact >> 
    commit_               >> 
    canvasZoomUpdateAll   >> 
    invalidateAll
  where 
    delpgact uhdl = forBoth' unboxBiAct (fsimple uhdl) . view currentCanvasInfo $ uhdl
    fsimple :: UnitHoodle -> CanvasInfo a -> MainCoroutine UnitHoodle
    fsimple uhdl cinfo = do 
      case view hoodleModeState uhdl of 
        ViewAppendState hdl -> do 
          hdl' <- liftIO $ deletePageInHoodle hdl 
                             (PageNum (view currentPageNum cinfo))
          liftIO . updatePageAll (ViewAppendState hdl')
                 . set hoodleModeState  (ViewAppendState hdl') $ uhdl
        SelectState _ -> do 
          msgShout "deleteCurrentPage: not implemented yet"
          return uhdl
      
-- | delete designated page
deletePageInHoodle :: Hoodle EditMode -> PageNum -> IO (Hoodle EditMode)
deletePageInHoodle hdl (PageNum pgn) = do 
  let pagelst = M.elems . view gpages $ hdl 
      (pagesbefore,_cpage:pagesafter) = splitAt pgn pagelst
      npagelst = pagesbefore ++ pagesafter
      nhdl = set gpages (M.fromList . zip [0..] $ npagelst) hdl
  return nhdl


-- | 
addNewPageInHoodle :: BackgroundStyle
                   -> AddDirection  
                   -> Hoodle EditMode
                   -> Int 
                   -> MainCoroutine (Hoodle EditMode)
addNewPageInHoodle bsty dir hdl cpn = do
    let pagelst = M.elems . view gpages $ hdl
        (pagesbefore,cpage:pagesafter) = splitAt cpn pagelst
        cbkg = view gbackground cpage
    nbkg <- newBkg bsty cbkg
    npage <- set gbackground nbkg <$> newPageFromOld cpage
    geometry <- liftIO . getGeometry4CurrCvs . view (unitHoodles.currentUnit) =<< get
    callRenderer_ (updateBkgCache geometry (PageNum cpn,npage))
    let npagelst = case dir of 
                     PageBefore -> pagesbefore ++ (npage : cpage : pagesafter)
                     PageAfter -> pagesbefore ++ (cpage : npage : pagesafter)
        nhdl = set gpages (M.fromList . zip [0..] $ npagelst) hdl
    return nhdl 


newBkg :: BackgroundStyle -> RBackground -> MainCoroutine RBackground 
newBkg bsty bkg = do
    xst <- get
    let npmode = xst ^. settings.newPageMode
        rhdl = (getHoodle . view (unitHoodles.currentUnit)) xst
        mtotN = pdfNumPages <$> (rhdl ^. gembeddedpdf)  
    let bstystr = convertBackgroundStyleToByteString bsty 
        defbkg = RBkgSmpl "white" bstystr <$> issueSurfaceID
    case npmode of 
      NPPlain -> defbkg
      NPLast -> case bkg of 
        RBkgSmpl c _ _ -> RBkgSmpl c bstystr <$> issueSurfaceID
        RBkgPDF d f n pg _ -> RBkgPDF d f n pg <$> issueSurfaceID
        RBkgEmbedPDF n pg _ -> RBkgEmbedPDF n pg <$> issueSurfaceID
      NPCycle -> 
        case mtotN of
          Nothing -> defbkg
          Just totN -> do
            let n1 = maybe 1 id (xst ^. nextPdfBkgPageNum)
            case findPDFBkg rhdl n1 of
                 Nothing -> defbkg
                 Just bkg' -> issueSurfaceID >>= \i -> do
                                let n' = if n1 >= totN then 1 else (n1+1)
                                put ((nextPdfBkgPageNum .~ Just n') xst)
                                return bkg' { rbkg_surfaceid = i }


findPDFBkg :: RHoodle -> Int -> Maybe RBackground
findPDFBkg rhdl n1 = 
    let bkgs = M.elems (rhdl ^. gpages)
        pagematch n (RBkgPDF _ _ n' _ _) = n == n'
        pagematch n (RBkgEmbedPDF n' _ _) = n == n'
        pagematch _ _ = False
        matched = (filter (pagematch n1) . map (^. gbackground)) bkgs
    in case matched of 
         [] -> Nothing
         b:_ -> Just b


-- | 
newPageFromOld :: Page EditMode -> MainCoroutine (Page EditMode)
newPageFromOld pg = do
    sfcid <- issueSurfaceID
    return . ( glayers .~ (fromNonEmptyList (emptyRLayer sfcid,[]))) $ pg


updateBkgCache :: CanvasGeometry -> (PageNum, Page EditMode) -> Renderer ()
updateBkgCache geometry (pnum,page) = do
  (handler,(qpdf,qgen)) <- ask
  let dim@(Dim w h) = page ^. gdimension 
      CvsCoord (x0,y0) = 
        (desktop2Canvas geometry . page2Desktop geometry) (pnum,PageCoord (0,0))
      CvsCoord (x1,y1) = 
        (desktop2Canvas geometry . page2Desktop geometry) (pnum,PageCoord (w,h))
      s = (x1-x0) / w 
      rbkg = page ^. gbackground
      bkg = rbkg2Bkg rbkg
      sfcid = rbkg_surfaceid rbkg 
  case rbkg of 
    RBkgSmpl {..} -> do
      cmdid <- issueGenCommandID
      (liftIO . atomically) (sendGenCommand qgen cmdid (BkgSmplScaled sfcid rbkg_color rbkg_style (Dim w h) (Dim (x1-x0) (y1-y0))))
      return ()
    _             -> F.forM_ (rbkg_popplerpage rbkg) $ \pg -> do
      cmdid <- issuePDFCommandID
      (liftIO . atomically) (sendPDFCommand qpdf cmdid 
        (RenderPageScaled sfcid pg (Dim w h) (Dim (x1-x0) (y1-y0))))
      return ()




