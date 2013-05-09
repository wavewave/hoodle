{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Link
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Link where

import           Control.Applicative
import           Control.Lens (at,view,set,(%~))
import           Control.Monad.State (get,put,modify,liftIO,guard,when)
import           Control.Monad.Trans.Maybe 
import qualified Data.ByteString.Char8 as B 
import           Data.Foldable (forM_)
import           Data.Monoid (mconcat)
import           Data.UUID.V4 (nextRandom)
import           Graphics.UI.Gtk hiding (get,set) 
import           System.FilePath 
-- from hoodle-platform
import           Control.Monad.Trans.Crtn.Event 
import           Control.Monad.Trans.Crtn.Queue 
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple (SVG(..))
import           Data.Hoodle.Zipper
import           Graphics.Hoodle.Render.Item 
import           Graphics.Hoodle.Render.Type 
import           Graphics.Hoodle.Render.Type.HitTest 
import           Graphics.Hoodle.Render.Util.HitTest 
-- from this package
import           Hoodle.Accessor
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.File 
import           Hoodle.Coroutine.Select.Clipboard
import           Hoodle.Coroutine.TextInput 
import           Hoodle.Device 
import           Hoodle.ModelAction.ContextMenu
import           Hoodle.ModelAction.Select
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import           Hoodle.Util 
import           Hoodle.View.Coordinate
import           Hoodle.View.Draw
--
import Prelude hiding (mapM_, mapM,forM_)

makeTextSVGFromStringAt :: String 
                           -> CanvasId 
                           -> HoodleState
                           -> CanvasCoordinate
                           -> IO (B.ByteString, BBox)
makeTextSVGFromStringAt str cid xst ccoord = do 
    rdr <- makePangoTextSVG str 
    geometry <- getCanvasGeometryCvsId cid xst 
    let mpgcoord = (desktop2Page geometry . canvas2Desktop geometry) ccoord 
    return $ case mpgcoord of 
               Nothing -> rdr 
               Just (_,PageCoord (x',y')) -> 
                 let bbox' = moveBBoxULCornerTo (x',y') (snd rdr) 
                 in (fst rdr,bbox')

-- | 
notifyLink :: CanvasId -> PointerCoord -> MainCoroutine () 
notifyLink cid pcoord = do 
    xst <- get 
    (boxAction (f xst) . getCanvasInfo cid) xst 
    -- liftIO $ putStrLn $ " here : " ++ show (view (unboxLens notifiedItem) ncinfo)
    -- let nxst =setCanvasInfo (cid,ncinfo) xst
    -- put nxst 
    -- boxAction rndr ncinfo
  where 
    {-
    rndr :: forall b. (ViewMode b) => CanvasInfo b -> MainCoroutine ()
    rndr cvsInfo = do 
      let cpn = PageNum . view currentPageNum $ cvsInfo
          arr = view (viewInfo.pageArrangement) cvsInfo              
          mnotifyitem = view notifiedItem cvsInfo
          canvas = view drawArea cvsInfo
      geometry <- liftIO $ makeCanvasGeometry cpn arr canvas
      forM_ mnotifyitem 
        (\(pnum,bbx) -> do 
            let bbx_desk = xformBBox (unDeskCoord . page2Desktop geometry . (pnum,) . PageCoord) bbx   
            invalidateInBBox (Just bbx_desk) Efficient cid)
    -}
    f :: forall b. (ViewMode b) => HoodleState -> CanvasInfo b -> MainCoroutine ()
    f xst cvsInfo = do 
      let cpn = PageNum . view currentPageNum $ cvsInfo
          arr = view (viewInfo.pageArrangement) cvsInfo              
          mnotifyitem = view notifiedItem cvsInfo
          canvas = view drawArea cvsInfo
      geometry <- liftIO $ makeCanvasGeometry cpn arr canvas
      mresult <- 
        case (desktop2Page geometry . device2Desktop geometry) pcoord of 
          Nothing -> return Nothing 
          Just (pnum,PageCoord (x,y)) -> do 
            let hdl = getHoodle xst
                mpage = view (gpages.at (unPageNum pnum)) hdl
            case mpage of
              Nothing -> return Nothing
              Just page -> do 
                let itms =  (view gitems . current . view glayers) page
                               -- rItmsInCurrLyr    
                    lnks = filter isLinkInRItem itms           
                    hlnks = hltFilteredBy (\itm->isPointInBBox (getBBox itm) (x,y)) lnks
                    hitted = takeHitted hlnks 
                case mnotifyitem of 
                  Nothing -> if ((not.null) hitted) 
                             then Just <$> newNotify cvsInfo geometry pnum (head hitted) Nothing  
                             else return Nothing 
                  Just (opnum,obbx) -> do
                    let obbx_desk = xformBBox (unDeskCoord . page2Desktop geometry . (opnum,) . PageCoord) obbx   
                    if pnum == opnum &&  isPointInBBox obbx (x,y) 
                      then return Nothing
                      else if ((not.null) hitted) 
                           then Just <$> newNotify cvsInfo geometry pnum (head hitted) (Just obbx_desk)
                           else return (Just (Nothing,obbx_desk))
      forM_ mresult (\(mnewnotified,bbx_desk) -> do
                      let ncinfobox = (unboxSet notifiedItem mnewnotified . getCanvasInfo cid) xst 
                      put (setCanvasInfo (cid,ncinfobox) xst)
                      invalidateInBBox (Just bbx_desk) Efficient cid )
    ----                                                                                      
    newNotify :: CanvasInfo a -> CanvasGeometry -> PageNum -> RItem -> Maybe BBox 
                 -> MainCoroutine (Maybe (PageNum,BBox),BBox) 
    newNotify cvsInfo geometry pnum lnk mobbx_desk = do        
      let bbx = getBBox lnk
          bbx_desk = xformBBox (unDeskCoord . page2Desktop geometry . (pnum,) . PageCoord) bbx
          nbbx_desk = maybe bbx_desk (\obbx_desk->unionBBox bbx_desk obbx_desk) mobbx_desk
      liftIO $ print (pnum,bbx)
      return (Just (pnum,bbx),nbbx_desk)
          

{- when ((not.null) hitted) $ do  
            let lnk = head hitted 
                bbx = getBBox lnk
                bbx_desk = xformBBox (unDeskCoord . page2Desktop geometry
                                      . (pnum,) . PageCoord) bbx
            invalidateInBBox (Just bbx_desk) Efficient cid 
            return (set notifiedItem (Just (pnum,bbx)) cvsInfo) -}


-- | got a link address (or embedded image) from drag and drop             
gotLink :: Maybe String -> (Int,Int) -> MainCoroutine () 
gotLink mstr (x,y) = do 
  xst <- get 
  liftIO $ print mstr 
  let cid = getCurrentCanvasId xst
  mr <- runMaybeT $ do 
    str <- (MaybeT . return) mstr 
    let (str1,rem1) = break (== ',') str 
    guard ((not.null) rem1)
    return (B.pack str1,tail rem1) 
  case mr of 
    Nothing -> do 
      mr2 <- runMaybeT $ do 
        str <- (MaybeT . return) mstr 
        (MaybeT . return) (urlParse str)
      liftIO $ putStrLn ("mr2= " ++ show mr2)
      case mr2 of  
        Nothing -> return ()
        Just (FileUrl file) -> do 
          let ext = takeExtension file 
          if ext == ".png" || ext == ".PNG" || ext == ".jpg" || ext == ".JPG" 
            then do 
              let isembedded = view (settings.doesEmbedImage) xst 
              nitm <- liftIO (cnstrctRItem =<< makeNewItemImage isembedded file) 
              geometry <- liftIO $ getCanvasGeometryCvsId cid xst               
              let ccoord = CvsCoord (fromIntegral x,fromIntegral y)
                  mpgcoord = (desktop2Page geometry . canvas2Desktop geometry) ccoord 
              insertItemAt mpgcoord nitm 
            else return ()  
        Just (HttpUrl url) -> do 
          case getSelectedItmsFromHoodleState xst of     
            Nothing -> do 
              liftIO $ print "here"
              uuidbstr <- liftIO $ B.pack . show <$> nextRandom              
              rdrbbx <- liftIO $ makeTextSVGFromStringAt url cid xst 
                                   (CvsCoord (fromIntegral x,fromIntegral y))
              linkInsert "simple" (uuidbstr,url) url rdrbbx
            Just hititms -> do 
              b <- okCancelMessageBox ("replace selected item with link to " ++ url  ++ "?")
              when b $ do 
                let ulbbox = (unUnion . mconcat . fmap (Union . Middle . getBBox)) hititms 
                case ulbbox of 
                  Middle bbox@(BBox (ulx,uly) (lrx,lry)) -> do 
                    svg <- liftIO $ makeSVGFromSelection hititms bbox
                    uuidbstr <- liftIO $ B.pack . show <$> nextRandom
                    deleteSelection 
                    linkInsert "simple" (uuidbstr,url) url (svg_render svg,bbox)  
                  _ -> return ()          
    Just (uuidbstr,fp) -> do 
      let fn = takeFileName fp 
      case getSelectedItmsFromHoodleState xst of     
        Nothing -> do 
          rdr <- liftIO (makePangoTextSVG fn) 
          geometry <- liftIO $ getCanvasGeometryCvsId cid xst 
          let ccoord = CvsCoord (fromIntegral x,fromIntegral y)
              mpgcoord = (desktop2Page geometry . canvas2Desktop geometry) ccoord 
              rdr' = case mpgcoord of 
                       Nothing -> rdr 
                       Just (_,PageCoord (x',y')) -> 
                         let bbox' = moveBBoxULCornerTo (x',y') (snd rdr) 
                         in (fst rdr,bbox')
          linkInsert "simple" (uuidbstr,fp) fn rdr' 
        Just hititms -> do 
          b <- okCancelMessageBox ("replace selected item with link to " ++ fn ++ "?")
          when b $ do 
            let ulbbox = (unUnion . mconcat . fmap (Union . Middle . getBBox)) hititms 
            case ulbbox of 
              Middle bbox@(BBox (ulx,uly) (lrx,lry)) -> do 
                svg <- liftIO $ makeSVGFromSelection hititms bbox
                uuid <- liftIO $ nextRandom
                let uuidbstr = B.pack (show uuid) 
                deleteSelection 
                linkInsert "simple" (uuidbstr,fp) fn (svg_render svg,bbox)  
              _ -> return ()          
  liftIO $ putStrLn "gotLink"
  liftIO $ print mstr 
  liftIO $ print (x,y)

-- | 
addLink :: MainCoroutine ()
addLink = do 
    mfilename <- fileChooser FileChooserActionOpen Nothing 
    modify (tempQueue %~ enqueue (action mfilename)) 
    minput <- go
    case minput of 
      Nothing -> return () 
      Just (str,fname) -> do 
        uuid <- liftIO $ nextRandom
        let uuidbstr = B.pack (show uuid)
        rdr <- liftIO (makePangoTextSVG str) 
        linkInsert "simple" (uuidbstr,fname) str rdr 
  where 
    go = do r <- nextevent
            case r of 
              AddLink minput -> return minput 
              UpdateCanvas cid -> -- this is temporary 
                                  (invalidateInBBox Nothing Efficient cid) >> go 
              _ -> go 
    action mfn = mkIOaction $ 
                   \_evhandler -> do 
                     dialog <- messageDialogNew Nothing [DialogModal]
                                 MessageQuestion ButtonsOkCancel "add link" 
                     vbox <- dialogGetUpper dialog
                     txtvw <- textViewNew
                     boxPackStart vbox txtvw PackGrow 0 
                     widgetShowAll dialog
                     res <- dialogRun dialog 
                     case res of 
                       ResponseOk -> do 
                         buf <- textViewGetBuffer txtvw 
                         (istart,iend) <- (,) <$> textBufferGetStartIter buf
                                              <*> textBufferGetEndIter buf
                         l <- textBufferGetText buf istart iend True
                         widgetDestroy dialog
                         return (UsrEv (AddLink ((l,) <$> mfn)))
                       _ -> do 
                         widgetDestroy dialog
                         return (UsrEv (AddLink Nothing))

                





