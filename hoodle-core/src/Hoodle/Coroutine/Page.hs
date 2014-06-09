{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Page 
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Page where

import           Control.Applicative
import           Control.Concurrent (forkIO)
import           Control.Lens (view,set,over, (.~), (^.) )
import           Control.Monad
import           Control.Monad.State
-- import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F
import qualified Data.IntMap as M
import           Data.UUID
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
changePage modifyfn = updateXState changePageAction 
                      >> adjustScrollbarWithGeometryCurrent
                      >> invalidateAllInBBox Nothing Efficient  
  where changePageAction xst = unboxBiAct (fsingle xst) (fcont xst) 
                               . (^. currentCanvasInfo) $ xst
        fsingle xstate cvsInfo = do 
          let xojst = view hoodleModeState $ xstate  
              npgnum = modifyfn (cvsInfo ^. currentPageNum)
              cid = view canvasId cvsInfo
              bsty = view backgroundStyle xstate 
          (b,npgnum',_,xojst') <- changePageInHoodleModeState bsty npgnum xojst
          xstate' <- liftIO $ updatePageAll xojst' xstate 
          ncvsInfo <- liftIO $ setPage xstate' (PageNum npgnum') cid
          let xstatefinal = (currentCanvasInfo .~ ncvsInfo) xstate'
          when b (commit xstatefinal)
          return xstatefinal 
        
        fcont xstate cvsInfo = do 
          let xojst = view hoodleModeState xstate  
              npgnum = modifyfn (cvsInfo ^. currentPageNum)
              cid  = cvsInfo ^. canvasId
              bsty = xstate  ^. backgroundStyle
          (b,npgnum',_selectedpage,xojst')
            <- changePageInHoodleModeState bsty npgnum xojst
          xstate' <- liftIO $ updatePageAll xojst' xstate 
          ncvsInfo <- liftIO $ setPage xstate' (PageNum npgnum') cid
          xstatefinal <- return . over currentCanvasInfo (const ncvsInfo) $ xstate'
          when b (commit xstatefinal)
          return xstatefinal 


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
          callRenderer $ \hdlr -> updateBkgCache hdlr npage >> return GotNone
          waitSomeEvent (\case RenderEv GotNone -> True ; _ -> False )
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
    updateXState zoomUpdateAction 
    adjustScrollbarWithGeometryCvsId cid
    hdl <- getHoodle <$> get
    F.forM_ (hdl ^. gpages) $ \pg -> callRenderer_ (\hdlr -> updateBkgCache hdlr pg)
--       callRenderer $ \hdlr -> updateBkgCache hdlr pg >> return GotNone
--      waitSomeEvent (\case RenderEv GotNone -> True ; _ -> False )
    renderfunc
  where zoomUpdateAction xst =  
          unboxBiAct (fsingle xst) (fcont xst) . getCanvasInfo cid $ xst 
        fsingle xstate cinfo = do   
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
          return . modifyCanvasInfo cid (const ncinfobox) $ xstate
        fcont xstate cinfo = do   
          geometry <- liftIO $ getCvsGeomFrmCvsInfo cinfo 
          let zmode = maybe (view (viewInfo.zoomMode) cinfo) id mzmode 
              cpn = PageNum $ view currentPageNum cinfo 
              cdim = canvasDim geometry 
              hdl = getHoodle xstate 
              origcoord = case mcoord of
                            Just coord -> coord 
                            Nothing -> either (const (cpn,PageCoord (0,0))) id 
                                         (getCvsOriginInPage geometry)
              narr = makeContinuousArrangement zmode cdim hdl origcoord
              ncinfobox = CanvasContPage
                          . (viewInfo.pageArrangement .~  narr)
                          . (viewInfo.zoomMode .~ zmode) $ cinfo
          return . modifyCanvasInfo cid (const ncinfobox) $ xstate

-- | 
canvasZoomUpdateCvsId :: CanvasId 
                         -> Maybe ZoomMode 
                         -> MainCoroutine ()
canvasZoomUpdateCvsId cid mzmode = 
  canvasZoomUpdateGenRenderCvsId invalidateAll cid mzmode Nothing
  
-- | 
canvasZoomUpdateBufAll :: MainCoroutine () 
canvasZoomUpdateBufAll = do 
    klst <- liftM (M.keys . getCanvasInfoMap) get
    mapM_ updatefunc klst 
  where 
    updatefunc cid 
      = canvasZoomUpdateGenRenderCvsId  (invalidateInBBox Nothing Efficient cid) cid Nothing Nothing 


-- |
canvasZoomUpdateAll :: MainCoroutine () 
canvasZoomUpdateAll = do 
  klst <- liftM (M.keys . getCanvasInfoMap) get
  mapM_ (flip canvasZoomUpdateCvsId Nothing) klst 


-- | 
canvasZoomUpdate :: Maybe ZoomMode -> MainCoroutine () 
canvasZoomUpdate mzmode = do  
  cid <- (liftM (getCurrentCanvasId) get)
  canvasZoomUpdateCvsId cid mzmode

-- |
pageZoomChange :: ZoomMode -> MainCoroutine () 
pageZoomChange = canvasZoomUpdate . Just 

-- | 
pageZoomChangeRel :: ZoomModeRel -> MainCoroutine () 
pageZoomChangeRel rzmode = do 
    forBoth' unboxBiAct fsingle . view currentCanvasInfo =<< get 
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
newPage dir = updateXState npgBfrAct 
              >> commit_ 
              >> canvasZoomUpdateAll 
              >> invalidateAll
  where 
    npgBfrAct xst = forBoth' unboxBiAct (fsimple xst) . view currentCanvasInfo $ xst
    fsimple :: HoodleState -> CanvasInfo a -> MainCoroutine HoodleState
    fsimple xstate cinfo = do 
      case view hoodleModeState xstate of 
        ViewAppendState hdl -> do 
          let bsty = view backgroundStyle xstate 
          hdl' <- addNewPageInHoodle bsty dir hdl (view currentPageNum cinfo)
          return =<< liftIO . updatePageAll (ViewAppendState hdl')
                     . set hoodleModeState  (ViewAppendState hdl') $ xstate 
        SelectState _ -> do 
          liftIO $ putStrLn " not implemented yet"
          return xstate
      
-- | delete current page of current canvas
deleteCurrentPage :: MainCoroutine ()           
deleteCurrentPage = do 
    updateXState delpgact >> commit_ >> canvasZoomUpdateAll >> invalidateAll
  where 
    delpgact xst = forBoth' unboxBiAct (fsimple xst) . view currentCanvasInfo $ xst
    fsimple :: HoodleState -> CanvasInfo a -> MainCoroutine HoodleState
    fsimple xstate cinfo = do 
      case view hoodleModeState xstate of 
        ViewAppendState hdl -> do 
          hdl' <- liftIO $ deletePageInHoodle hdl 
                             (PageNum (view currentPageNum cinfo))
          return =<< liftIO . updatePageAll (ViewAppendState hdl')
                     . set hoodleModeState  (ViewAppendState hdl') $ xstate 
        SelectState _ -> do 
          liftIO $ putStrLn " not implemented yet"
          return xstate
      
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
    callRenderer_ (flip updateBkgCache npage)
    let npagelst = case dir of 
                     PageBefore -> pagesbefore ++ (npage : cpage : pagesafter)
                     PageAfter -> pagesbefore ++ (cpage : npage : pagesafter)
        nhdl = set gpages (M.fromList . zip [0..] $ npagelst) hdl
    return nhdl 


newBkg :: BackgroundStyle -> RBackground -> MainCoroutine RBackground 
newBkg bsty bkg = do
    let bstystr = convertBackgroundStyleToByteString bsty 
    case bkg of 
      RBkgSmpl c _ _ -> RBkgSmpl c bstystr <$> liftIO nextRandom
      _              -> RBkgSmpl "white" bstystr <$> liftIO nextRandom


-- | 
newPageFromOld :: Page EditMode -> MainCoroutine (Page EditMode)
newPageFromOld =
    return . ( glayers .~ (fromNonEmptyList (emptyRLayer,[])))


updateBkgCache :: ((UUID, Maybe Cairo.Surface) -> IO ()) 
                        -> Page EditMode -> IO ()
updateBkgCache handler page = do
  let rbkg = page ^. gbackground
      dim@(Dim w h) = page ^. gdimension 
  case rbkg of 
    RBkgSmpl _ _ uuid -> do 
      let bkg = rbkg2Bkg rbkg
      putStrLn $ "updating " ++ show uuid
      forkIO $ do
        sfc <- Cairo.createImageSurface Cairo.FormatARGB32 (floor w) (floor h)
        Cairo.renderWith sfc $ renderBkg (bkg,dim)
        handler (uuid, Just sfc)
      return ()
        
    _ -> return () 
