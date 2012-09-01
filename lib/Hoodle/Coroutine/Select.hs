-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Select 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Select where

-- from other package 
import           Control.Applicative 
import           Control.Category
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Trans
import           Control.Monad.Coroutine 
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import           Data.IORef
import           Data.Label
import           Data.Monoid 
import qualified Data.IntMap as M
import           Data.Sequence (Seq,(|>))
import qualified Data.Sequence as Sq (empty)
import           Data.Time.Clock
import qualified Data.Serialize as Se 
import           Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.Matrix as Mat
import           Graphics.UI.Gtk hiding (get,set,disconnect)
-- from hoodle-platform
import           Data.Xournal.Simple (Dimension(..))
import           Data.Xournal.Generic
import           Data.Xournal.BBox
import           Graphics.Xournal.Render.Type
import           Graphics.Xournal.Render.BBoxMapPDF
import           Graphics.Xournal.Render.HitTest
import           Graphics.Xournal.Render.BBox
import           Graphics.Xournal.Render.Generic
-- from this package
import           Hoodle.Type.Event 
import           Hoodle.Type.Enum
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Canvas
import           Hoodle.Type.Clipboard
import           Hoodle.Type.PageArrangement
import           Hoodle.Type.XournalState
import           Hoodle.Type.Alias
import           Hoodle.Accessor
import           Hoodle.Device
import           Hoodle.View.Draw
import           Hoodle.View.Coordinate
import           Hoodle.Coroutine.EventConnect
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Pen
import           Hoodle.Coroutine.Mode
import           Hoodle.Coroutine.Commit
import           Hoodle.ModelAction.Page
import           Hoodle.ModelAction.Select
import           Hoodle.ModelAction.Layer 
import           Hoodle.Script.Hook 
-- 
import           Prelude hiding ((.), id)

-- |

createTempSelectRender :: PageNum -> CanvasGeometry -> Page EditMode
                          -> a 
                          -> MainCoroutine (TempSelectRender a) 
createTempSelectRender pnum geometry page x = do 
    let Dim cw ch = unCanvasDimension . canvasDim $ geometry
        xformfunc = cairoXform4PageCoordinate geometry pnum 
        renderfunc = do   
          xformfunc 
          cairoRenderOption (InBBoxOption Nothing) (InBBox page) 
          return ()
    tempsurface <- liftIO $ createImageSurface FormatARGB32 (floor cw) (floor ch)  
    let tempselection = TempSelectRender tempsurface (cw,ch) x
    liftIO $ updateTempSelection tempselection renderfunc True
    return tempselection 


-- | For Selection mode from pen mode with 2nd pen button
    
dealWithOneTimeSelectMode :: MainCoroutine () -> MainCoroutine ()
dealWithOneTimeSelectMode action = do 
  xstate <- getSt 
  case get isOneTimeSelectMode xstate of 
    NoOneTimeSelectMode -> action 
    YesBeforeSelect -> 
      action >> updateXState (return . set isOneTimeSelectMode YesAfterSelect)
    YesAfterSelect -> do 
      updateXState (return . set isOneTimeSelectMode NoOneTimeSelectMode) 
      modeChange ToViewAppendMode

-- | main mouse pointer click entrance in rectangular selection mode. 
--   choose either starting new rectangular selection or move previously 
--   selected selection. 

selectRectStart :: CanvasId -> PointerCoord -> MainCoroutine ()
selectRectStart cid = commonPenStart rectaction cid
  where rectaction cinfo pnum geometry (cidup,cidmove) (x,y) = do
          strs <- getAllStrokeBBoxInCurrentLayer
          ctime <- liftIO $ getCurrentTime
          let newSelectAction page = 
                dealWithOneTimeSelectMode $ do 
                  tsel <- createTempSelectRender pnum geometry page [] 
                  newSelectRectangle cid pnum geometry cidmove cidup strs 
                                     (x,y) ((x,y),ctime) tsel
                  surfaceFinish (tempSurface tsel)          
          let 
              action (Right tpage) | hitInHandle tpage (x,y) = 
                case getULBBoxFromSelected tpage of 
                  Middle bbox ->  
                    maybe (return ()) 
                          (\handle -> startResizeSelect 
                                        handle cid pnum geometry cidmove cidup 
                                        bbox ((x,y),ctime) tpage)
                          (checkIfHandleGrasped bbox (x,y))
                  _ -> return () 
              action (Right tpage) | hitInSelection tpage (x,y) = do
                startMoveSelect cid pnum geometry cidmove cidup ((x,y),ctime) tpage
              action (Right tpage) | otherwise = newSelectAction (gcast tpage :: Page EditMode )
              action (Left page) = newSelectAction page
          xstate <- getSt 
          let xojstate = get xournalstate xstate 
          let epage = getCurrentPageEitherFromXojState cinfo xojstate 
          action epage

-- | 

newSelectRectangle :: CanvasId
                   -> PageNum 
                   -> CanvasGeometry
                   -> ConnectId DrawingArea -> ConnectId DrawingArea
                   -> [StrokeBBox] 
                   -> (Double,Double)
                   -> ((Double,Double),UTCTime)
                   -> TempSelection 
                   -> MainCoroutine () 
newSelectRectangle cid pnum geometry connidmove connidup strs orig 
                   (prev,otime) tempselection = do  
    r <- await 
    xst <- getSt 
    boxAction (fsingle r xst) . getCanvasInfo cid $ xst
  where 
    fsingle r xstate cinfo = penMoveAndUpOnly r pnum geometry defact
                               (moveact xstate cinfo) (upact xstate cinfo)
    defact = newSelectRectangle cid pnum geometry connidmove connidup strs orig 
                         (prev,otime) tempselection 
    moveact _xstate _cinfo (_pcoord,(x,y)) = do 
      let bbox = BBox orig (x,y)
          hittestbbox = mkHitTestInsideBBox bbox strs
          hittedstrs = concat . map unHitted . getB $ hittestbbox
      page <- getCurrentPageCvsId cid
      let (fstrs,sstrs) = separateFS $ getDiffStrokeBBox (tempSelected tempselection) hittedstrs 
      (willUpdate,(ncoord,ntime)) <- liftIO $ getNewCoordTime (prev,otime) (x,y)
      when ((not.null) fstrs || (not.null) sstrs ) $ do 
        let xformfunc = cairoXform4PageCoordinate geometry pnum 
            ulbbox = unUnion . mconcat . fmap (Union .Middle . flip inflate 5 . strokebbox_bbox) $ fstrs
            renderfunc = do   
              xformfunc 
              case ulbbox of 
                Top -> do 
                  cairoRenderOption (InBBoxOption Nothing) (InBBox page) 
                  mapM_ renderSelectedStroke hittedstrs
                Middle sbbox -> do 
                  let redrawee = filter (hitTestBBoxBBox sbbox.strokebbox_bbox) hittedstrs  
                  cairoRenderOption (InBBoxOption (Just sbbox)) (InBBox page)
                  clipBBox (Just sbbox)
                  mapM_ renderSelectedStroke redrawee 
                Bottom -> return ()
              mapM_ renderSelectedStroke sstrs 
        liftIO $ updateTempSelection tempselection renderfunc False
      when willUpdate $  
        invalidateTemp cid (tempSurface tempselection) 
                           (renderBoxSelection bbox) 
      newSelectRectangle cid pnum geometry connidmove connidup strs orig 
                         (ncoord,ntime)
                         tempselection { tempSelectInfo = hittedstrs }
    upact xstate cinfo pcoord = do       
      let (_,(x,y)) = runIdentity $ 
            skipIfNotInSamePage pnum geometry pcoord 
                                (return (pcoord,prev)) return
          epage = getCurrentPageEitherFromXojState cinfo (get xournalstate xstate)
          cpn = get currentPageNum cinfo 
          bbox = BBox orig (x,y)
          hittestbbox = mkHitTestInsideBBox bbox strs
          selectstrs = fmapAL unNotHitted id hittestbbox
          SelectState txoj = get xournalstate xstate
          newpage = case epage of 
                      Left pagebbox -> makePageSelectMode pagebbox selectstrs 
                      Right tpage -> 
                        let ls = glayers tpage 
                            currlayer = gselectedlayerbuf ls
                            newlayer = GLayerBuf (get g_buffer currlayer) (TEitherAlterHitted (Right selectstrs))
                            npage = tpage { glayers = ls { gselectedlayerbuf = newlayer } } 
                        in npage
          newtxoj = txoj { gselectSelected = Just (cpn,newpage) } 
          ui = get gtkUIManager xstate
      liftIO $ toggleCutCopyDelete ui (isAnyHitted  selectstrs)
      putSt . set xournalstate (SelectState newtxoj) 
            =<< (liftIO (updatePageAll (SelectState newtxoj) xstate))
      disconnect connidmove
      disconnect connidup 
      invalidateAll 


-- | prepare for moving selection 
      
startMoveSelect :: CanvasId 
                   -> PageNum 
                   -> CanvasGeometry 
                   -> ConnectId DrawingArea
                   -> ConnectId DrawingArea
                   -> ((Double,Double),UTCTime) 
                   -> Page SelectMode
                   -> MainCoroutine () 
startMoveSelect cid pnum geometry cidmove cidup ((x,y),ctime) tpage = do  
    strimage <- liftIO $ mkStrokesNImage geometry tpage
    tsel <- createTempSelectRender pnum geometry
              (gcast tpage :: Page EditMode) 
              strimage 
    moveSelect cid pnum geometry cidmove cidup (x,y) ((x,y),ctime) tsel 
    surfaceFinish (tempSurface tsel)                  
    surfaceFinish (imageSurface strimage)

-- | 
         
moveSelect :: CanvasId
              -> PageNum -- ^ starting pagenum 
              -> CanvasGeometry
              -> ConnectId DrawingArea 
              -> ConnectId DrawingArea
              -> (Double,Double)
              -> ((Double,Double),UTCTime)
              -> TempSelectRender StrokesNImage
              -> MainCoroutine ()
moveSelect cid pnum geometry connidmove connidup orig@(x0,y0) 
           (prev,otime) tempselection = do
    xst <- getSt
    r <- await 
    boxAction (fsingle r xst) . getCanvasInfo cid $ xst 
  where 
    fsingle r xstate cinfo = 
      penMoveAndUpInterPage r pnum geometry defact (moveact xstate cinfo) (upact xstate cinfo) 
    defact = moveSelect cid pnum geometry connidmove connidup orig (prev,otime) 
               tempselection
    moveact _xstate _cinfo oldpgn pcpair@(newpgn,PageCoord (px,py)) = do 
      let (x,y) 
            | oldpgn == newpgn = (px,py) 
            | otherwise = 
              let DeskCoord (xo,yo) = page2Desktop geometry (oldpgn,PageCoord (0,0))
                  DeskCoord (xn,yn) = page2Desktop geometry pcpair 
              in (xn-xo,yn-yo)
      
      (willUpdate,(ncoord,ntime)) <- liftIO $ getNewCoordTime (prev,otime) (x,y) 
      when willUpdate $ do 
        let sfunc = offsetFunc (x-x0,y-y0)
            xform = unCvsCoord . desktop2Canvas geometry
                    . page2Desktop geometry . (,) pnum . PageCoord
            (c1,c2) = xform (sfunc (0,0))
            (a1',a2') = xform (sfunc (1,0))
            (a1,a2) = (a1'-c1,a2'-c2)
            (b1',b2') = xform (sfunc (0,1))
            (b1,b2) = (b1'-c1,b2'-c2)
            xformmat = Mat.Matrix a1 a2 b1 b2 c1 c2 
        invalidateTempBasePage cid (tempSurface tempselection) pnum 
          (drawTempSelectImage geometry tempselection xformmat) 
      moveSelect cid pnum geometry connidmove connidup orig (ncoord,ntime) 
        tempselection
    upact :: (ViewMode a) => HoodleState -> CanvasInfo a -> PointerCoord -> MainCoroutine () 
    upact xst cinfo pcoord =  
      switchActionEnteringDiffPage pnum geometry pcoord (return ()) 
        (chgaction xst cinfo) 
        (ordaction xst cinfo)
    chgaction :: (ViewMode a) => HoodleState -> CanvasInfo a -> PageNum -> (PageNum,PageCoordinate) -> MainCoroutine () 
    chgaction xstate cinfo oldpgn (newpgn,PageCoord (x,y)) = do 
      let xojstate@(SelectState txoj) = get xournalstate xstate
          epage = getCurrentPageEitherFromXojState cinfo xojstate
      (xstate1,ntxoj1,selectedstrs) <- 
        case epage of 
          Right oldtpage -> do 
            let strs = getSelectedStrokes oldtpage
            let oldtpage' = deleteSelected oldtpage
            ntxoj <- liftIO $ updateTempXournalSelectIO txoj oldtpage' (unPageNum oldpgn)
            xst <- return . set xournalstate (SelectState ntxoj)
                     =<< (liftIO (updatePageAll (SelectState ntxoj) xstate)) 
            return (xst,ntxoj,strs)       
          Left _ -> error "this is impossible, in moveSelect" 
      let maction = do 
            page <- M.lookup (unPageNum newpgn) (get g_selectAll ntxoj1)
            let (mcurrlayer,npage) = getCurrentLayerOrSet page
            currlayer <- mcurrlayer 
            let oldstrs = get g_bstrokes currlayer
            let newstrs = map (changeStrokeBy (offsetFunc (x-x0,y-y0))) selectedstrs 
                alist = oldstrs :- Hitted newstrs :- Empty 
                ntpage = makePageSelectMode npage alist  
                coroutineaction = do 
                  ntxoj2 <- liftIO $ updateTempXournalSelectIO ntxoj1 ntpage (unPageNum newpgn)  
                  let ncinfo = set currentPageNum (unPageNum newpgn) $ cinfo 
                      cmap = getCanvasInfoMap xstate1 
                      cmap' = M.adjust (const (CanvasInfoBox ncinfo)) cid cmap
                      xst = maybe xstate1 id $ setCanvasInfoMap cmap' xstate1
                  return . set xournalstate (SelectState ntxoj2)
                    =<< (liftIO (updatePageAll (SelectState ntxoj2) xst)) 
            return coroutineaction
      xstate2 <- maybe (return xstate1) id maction 
      commit xstate2
      disconnect connidmove
      disconnect connidup 
      invalidateAll 
      
    ordaction xstate cinfo _pgn (_cpn,PageCoord (x,y)) = do 
      let offset = (x-x0,y-y0)
          xojstate@(SelectState txoj) = get xournalstate xstate
          epage = getCurrentPageEitherFromXojState cinfo xojstate
          pagenum = get currentPageNum cinfo
      case epage of 
        Right tpage -> do 
          let newtpage = changeSelectionByOffset offset tpage 
          newtxoj <- liftIO $ updateTempXournalSelectIO txoj newtpage pagenum 
          commit . set xournalstate (SelectState newtxoj)
                 =<< (liftIO (updatePageAll (SelectState newtxoj) xstate))
        Left _ -> error "this is impossible, in moveSelect" 
      disconnect connidmove
      disconnect connidup 
      invalidateAll 
      

-- | prepare for resizing selection 
      
startResizeSelect :: Handle 
                     -> CanvasId 
                     -> PageNum 
                     -> CanvasGeometry 
                     -> ConnectId DrawingArea
                     -> ConnectId DrawingArea
                     -> BBox
                     -> ((Double,Double),UTCTime) 
                     -> Page SelectMode
                     -> MainCoroutine () 
startResizeSelect handle cid pnum geometry cidmove cidup bbox 
                  ((x,y),ctime) tpage = do  
    strimage <- liftIO $ mkStrokesNImage geometry tpage  
    tsel <- createTempSelectRender pnum geometry 
              (gcast tpage :: Page EditMode) 
              strimage 
    resizeSelect handle cid pnum geometry cidmove cidup bbox ((x,y),ctime) tsel 
    surfaceFinish (tempSurface tsel)  
    surfaceFinish (imageSurface strimage)
  

-- | 
      
resizeSelect :: Handle 
                -> CanvasId
                -> PageNum 
                -> CanvasGeometry
                -> ConnectId DrawingArea 
                -> ConnectId DrawingArea
                -> BBox
                -> ((Double,Double),UTCTime)
                -> TempSelectRender StrokesNImage
                -> MainCoroutine ()
resizeSelect handle cid pnum geometry connidmove connidup origbbox 
             (prev,otime) tempselection = do
    xst <- getSt
    r <- await 
    boxAction (fsingle r xst) . getCanvasInfo cid $ xst
  where
    fsingle r xstate cinfo = penMoveAndUpOnly r pnum geometry defact (moveact xstate cinfo) (upact xstate cinfo)
    defact = resizeSelect handle cid pnum geometry connidmove connidup 
               origbbox (prev,otime) tempselection
    moveact _xstate _cinfo (_pcoord,(x,y)) = do 
      (willUpdate,(ncoord,ntime)) <- liftIO $ getNewCoordTime (prev,otime) (x,y) 
      when willUpdate $ do 
        let newbbox = getNewBBoxFromHandlePos handle origbbox (x,y)      
            sfunc = scaleFromToBBox origbbox newbbox
            xform = unCvsCoord . desktop2Canvas geometry
                    . page2Desktop geometry . (,) pnum . PageCoord
            (c1,c2) = xform (sfunc (0,0))
            (a1',a2') = xform (sfunc (1,0))
            (a1,a2) = (a1'-c1,a2'-c2)
            (b1',b2') = xform (sfunc (0,1))
            (b1,b2) = (b1'-c1,b2'-c2)
            xformmat = Mat.Matrix a1 a2 b1 b2 c1 c2 
        invalidateTemp cid (tempSurface tempselection) 
                           (drawTempSelectImage geometry tempselection 
                              xformmat)
      resizeSelect handle cid pnum geometry connidmove connidup 
                   origbbox (ncoord,ntime) tempselection
    upact xstate cinfo pcoord = do 
      let (_,(x,y)) = runIdentity $ 
            skipIfNotInSamePage pnum geometry pcoord 
                                (return (pcoord,prev)) return
          newbbox = getNewBBoxFromHandlePos handle origbbox (x,y)
          xojstate@(SelectState txoj) = get xournalstate xstate
          epage = getCurrentPageEitherFromXojState cinfo xojstate
          pagenum = get currentPageNum cinfo
      case epage of 
        Right tpage -> do 
          let sfunc = scaleFromToBBox origbbox newbbox
          let newtpage = changeSelectionBy sfunc tpage 
          newtxoj <- liftIO $ updateTempXournalSelectIO txoj newtpage pagenum 
          commit . set xournalstate (SelectState newtxoj)
                 =<< (liftIO (updatePageAll (SelectState newtxoj) xstate))
        Left _ -> error "this is impossible, in resizeSelect" 
      disconnect connidmove
      disconnect connidup 
      invalidateAll
      return ()    

-- |

deleteSelection :: MainCoroutine ()
deleteSelection = do 
  liftIO $ putStrLn "delete selection is called"
  xstate <- getSt
  let SelectState txoj = get xournalstate xstate 
      Just (n,tpage) = gselectSelected txoj
      slayer = gselectedlayerbuf . glayers $ tpage
  case unTEitherAlterHitted . get g_bstrokes $ slayer of 
    Left _ -> liftIO $ putStrLn "no stroke selection 2 "
    Right alist -> do 
      let newlayer = Left . concat . getA $ alist
          oldlayers = glayers tpage
          newpage = tpage { glayers = oldlayers { gselectedlayerbuf = GLayerBuf (get g_buffer slayer) (TEitherAlterHitted newlayer) } } 
      newtxoj <- liftIO $ updateTempXournalSelectIO txoj newpage n          
      newxstate <- liftIO $ updatePageAll (SelectState newtxoj) 
                            . set xournalstate (SelectState newtxoj)
                            $ xstate 
      commit newxstate 
      let ui = get gtkUIManager newxstate
      liftIO $ toggleCutCopyDelete ui False 
      invalidateAll 
          
-- | 

cutSelection :: MainCoroutine () 
cutSelection = do
  liftIO $ putStrLn "cutSelection called"
  copySelection 
  deleteSelection

-- | 
  
updateClipboard :: HoodleState -> [StrokeBBox] -> IO HoodleState 
updateClipboard xstate strs 
  | null strs = return xstate
  | otherwise = do 
    let ui = get gtkUIManager xstate
    hdltag <- atomNew "hoodle"
    tgttag <- atomNew "Stroke"
    seltag <- atomNew "Stroke"
    clip <- clipboardGet hdltag
    let bstr = C8.unpack . B64.encode . Se.encode $ strs 
    clipboardSetText clip bstr
    togglePaste ui True 
    case (get hookSet xstate) of 
      Nothing -> return () 
      Just hset -> case afterUpdateClipboardHook hset of 
                     Nothing -> return () 
                     Just uchook -> liftIO $ uchook strs 

    return xstate


  
-- | 

copySelection :: MainCoroutine ()
copySelection = do 
    updateXState copySelectionAction >> invalidateAll 
  where copySelectionAction xst = 
          boxAction (fsingle xst) . get currentCanvasInfo $ xst
        fsingle xstate cinfo = maybe (return xstate) id $ do  
          let xojstate = get xournalstate xstate
          let epage = getCurrentPageEitherFromXojState cinfo xojstate
          eitherMaybe epage `pipe` getActiveLayer 
                            `pipe` (Right . liftIO . updateClipboard xstate . takeHittedStrokes)
          where eitherMaybe (Left _) = Nothing
                eitherMaybe (Right a) = Just a 
                x `pipe` a = x >>= eitherMaybe . a 
                infixl 6 `pipe`



-- |


callback4Clip :: (MyEvent -> IO ()) -> IORef (Maybe [StrokeBBox]) -> Maybe String -> IO ()
callback4Clip callbk ref Nothing = do 
    writeIORef ref Nothing
    callbk (GotClipboardContent Nothing)
callback4Clip callbk ref (Just str) = do
    let r = do let bstr = C8.pack str 
               bstr' <- B64.decode bstr
               Se.decode bstr' 
    case r of 
      Left err -> do 
        putStrLn err >> writeIORef ref Nothing
        callbk (GotClipboardContent Nothing)
      Right cnt -> do  
        cnt `seq` writeIORef ref (Just cnt)
        callbk (GotClipboardContent (Just cnt))

-- |

getClipFromGtk :: MainCoroutine (Maybe [StrokeBBox])
getClipFromGtk = do 
    hdltag <- liftIO $ atomNew "hoodle"
    clip <- liftIO $ clipboardGet hdltag
    ref <- liftIO $ newIORef Nothing 
    callbk <- get callBack <$> getSt     
    liftIO $ clipboardRequestText clip (callback4Clip callbk ref)
    cnt <- liftIO $ readIORef ref
    case cnt of 
      Nothing -> do 
        r <- await 
        case r of 
          GotClipboardContent cnt' -> return cnt' 
          _ -> return Nothing 
      Just _ -> return cnt

-- | 

pasteToSelection :: MainCoroutine () 
pasteToSelection = do 
    mstrks <- getClipFromGtk 
    case mstrks of 
      Nothing -> return () 
      Just strks -> do 
        modeChange ToSelectMode >>updateXState (pasteAction strks) >> invalidateAll  
  where pasteAction stks xst = boxAction (fsimple stks xst) . get currentCanvasInfo 
                               $ xst
        fsimple stks xstate cinfo = do 
          geometry <- liftIO (getGeometry4CurrCvs xstate)
          let pagenum = get currentPageNum cinfo 
              xojstate@(SelectState txoj) = get xournalstate xstate
              nclipstrs = adjustStrokePosition4Paste geometry (PageNum pagenum) stks
              epage = getCurrentPageEitherFromXojState cinfo xojstate 
              tpage = either gcast id epage
              layerselect = gselectedlayerbuf . glayers $ tpage 
              ls  = glayers tpage
              gbuf = get g_buffer layerselect
              newlayerselect = case getActiveLayer tpage of 
                Left strs -> (GLayerBuf gbuf . TEitherAlterHitted . Right) (strs :- Hitted nclipstrs :- Empty)
                Right alist -> (GLayerBuf gbuf . TEitherAlterHitted . Right) 
                               (concat (interleave id unHitted alist) 
                                 :- Hitted nclipstrs 
                                 :- Empty )
              tpage' = tpage { glayers = ls { gselectedlayerbuf = newlayerselect } } 
          txoj' <- liftIO $ updateTempXournalSelectIO txoj tpage' pagenum 
          xstate' <- liftIO $ updatePageAll (SelectState txoj') 
                              . set xournalstate (SelectState txoj') 
                              $ xstate 
          commit xstate' 
          let ui = get gtkUIManager xstate' 
          liftIO $ toggleCutCopyDelete ui True
          return xstate' 
        
-- | 
  
selectPenColorChanged :: PenColor -> MainCoroutine () 
selectPenColorChanged pcolor = do 
  liftIO $ putStrLn "selectPenColorChanged called"
  xstate <- getSt
  let SelectState txoj = get xournalstate xstate 
      Just (n,tpage) = gselectSelected txoj
      slayer = gselectedlayerbuf . glayers $ tpage
  case unTEitherAlterHitted . get g_bstrokes $ slayer of 
    Left _ -> liftIO $ putStrLn "no stroke selection 2 "
    Right alist -> do 
      let alist' = fmapAL id 
                     (Hitted . map (changeStrokeColor pcolor) . unHitted) alist
          newlayer = Right alist'
          ls = glayers tpage 
          newpage = tpage { glayers = ls { gselectedlayerbuf = GLayerBuf (get g_buffer slayer) (TEitherAlterHitted newlayer) }} 
      newtxoj <- liftIO $ updateTempXournalSelectIO txoj newpage n
      commit =<< liftIO (updatePageAll (SelectState newtxoj)
                        . set xournalstate (SelectState newtxoj) $ xstate )
      invalidateAll 
          
-- | 

selectPenWidthChanged :: Double -> MainCoroutine () 
selectPenWidthChanged pwidth = do 
  liftIO $ putStrLn "selectPenWidthChanged called"
  xstate <- getSt
  let SelectState txoj = get xournalstate xstate 
      Just (n,tpage) = gselectSelected txoj
      slayer = gselectedlayerbuf . get g_layers $ tpage
  case unTEitherAlterHitted . get g_bstrokes $ slayer  of 
    Left _ -> liftIO $ putStrLn "no stroke selection 2 "
    Right alist -> do 
      let alist' = fmapAL id 
                     (Hitted . map (changeStrokeWidth pwidth) . unHitted) alist
          newlayer = Right alist'
          ls = get g_layers tpage 
          newpage = tpage { glayers = ls { gselectedlayerbuf = GLayerBuf (get g_buffer slayer) (TEitherAlterHitted newlayer) }} 
      newtxoj <- liftIO $ updateTempXournalSelectIO txoj newpage n          
      commit =<< liftIO (updatePageAll (SelectState newtxoj) 
                         . set xournalstate (SelectState newtxoj) $ xstate )
      invalidateAll 

-- | main mouse pointer click entrance in lasso selection mode. 
--   choose either starting new rectangular selection or move previously 
--   selected selection. 

selectLassoStart :: CanvasId -> PointerCoord -> MainCoroutine ()
selectLassoStart cid = commonPenStart lassoAction cid 
  where lassoAction cinfo pnum geometry (cidup,cidmove) (x,y) = do 
          strs <- getAllStrokeBBoxInCurrentLayer
          ctime <- liftIO $ getCurrentTime
          let newSelectAction page =    
                dealWithOneTimeSelectMode $ do 
                  tsel <- createTempSelectRender pnum geometry page [] 
                  newSelectLasso cinfo pnum geometry cidmove cidup strs 
                                 (x,y) ((x,y),ctime) (Sq.empty |> (x,y)) tsel
                  surfaceFinish (tempSurface tsel)          
          let action (Right tpage) | hitInSelection tpage (x,y) = 
                startMoveSelect cid pnum geometry cidmove cidup ((x,y),ctime) tpage
              action (Right tpage) | hitInHandle tpage (x,y) = 
                case getULBBoxFromSelected tpage of 
                  Middle bbox ->  
                    maybe (return ()) 
                          (\handle -> startResizeSelect 
                                        handle cid pnum geometry cidmove cidup 
                                        bbox ((x,y),ctime) tpage)
                          (checkIfHandleGrasped bbox (x,y))
                  _ -> return () 
              action (Right tpage) | otherwise = newSelectAction (gcast tpage :: Page EditMode )
              action (Left page) = newSelectAction page
          xstate <- getSt 
          let xojstate = get xournalstate xstate 
          let epage = getCurrentPageEitherFromXojState cinfo xojstate 
          action epage
          
-- | 

newSelectLasso :: (ViewMode a) => CanvasInfo a
                  -> PageNum 
                  -> CanvasGeometry
                  -> ConnectId DrawingArea -> ConnectId DrawingArea
                  -> [StrokeBBox] 
                  -> (Double,Double)
                  -> ((Double,Double),UTCTime)
                  -> Seq (Double,Double)
                  -> TempSelection 
                  -> MainCoroutine ()
newSelectLasso cvsInfo pnum geometry cidmove cidup strs orig (prev,otime) lasso tsel = do
    r <- await 
    fsingle r cvsInfo 
  where  
    fsingle r cinfo = penMoveAndUpOnly r pnum geometry defact
                        (moveact cinfo) (upact cinfo)
    defact = newSelectLasso cvsInfo pnum geometry cidmove cidup strs orig 
               (prev,otime) lasso tsel
    moveact cinfo (_pcoord,(x,y)) = do 
      let nlasso = lasso |> (x,y)
      (willUpdate,(ncoord,ntime)) <- liftIO $ getNewCoordTime (prev,otime) (x,y)
      when willUpdate $ do 
        invalidateTemp (get canvasId cinfo) (tempSurface tsel) (renderLasso nlasso) 
      newSelectLasso cinfo pnum geometry cidmove cidup strs orig 
                     (ncoord,ntime) nlasso tsel
    upact cinfo pcoord = do 
      xstate <- getSt 
      let (_,(x,y)) = runIdentity $ 
            skipIfNotInSamePage pnum geometry pcoord 
                                (return (pcoord,prev)) return
          nlasso = lasso |> (x,y)
          xojstate = get xournalstate xstate 
          epage = getCurrentPageEitherFromXojState cinfo xojstate
          cpn = get currentPageNum cinfo 
          hittestlasso = mkHitTestAL (hitLassoStroke (nlasso |> orig)) strs
          selectstrs = fmapAL unNotHitted id hittestlasso
          SelectState txoj = get xournalstate xstate
          newpage = case epage of 
                      Left pagebbox -> 
                        let (mcurrlayer,npagebbox) = getCurrentLayerOrSet pagebbox
                            currlayer = maybe (error "newSelectLasso") id mcurrlayer 
                            newlayer = GLayerBuf (get g_buffer currlayer) (TEitherAlterHitted (Right selectstrs))
                            tpg = gcast npagebbox 
                            ls = get g_layers tpg 
                            npg = tpg { glayers = ls { gselectedlayerbuf = newlayer}  }
                        in npg 
                      Right tpage -> 
                        let ls = glayers tpage 
                            currlayer = gselectedlayerbuf ls
                            newlayer = GLayerBuf (get g_buffer currlayer) (TEitherAlterHitted (Right selectstrs))
                            npage = tpage { glayers = ls { gselectedlayerbuf = newlayer } } 
                        in npage
          newtxoj = txoj { gselectSelected = Just (cpn,newpage) } 
      let ui = get gtkUIManager xstate
      liftIO $ toggleCutCopyDelete ui (isAnyHitted  selectstrs)
      putSt . set xournalstate (SelectState newtxoj) 
            =<< (liftIO (updatePageAll (SelectState newtxoj) xstate))
      disconnect cidmove
      disconnect cidup 
      invalidateAll 


