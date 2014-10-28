{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Link
-- Copyright   : (c) 2013, 2014 Ian-Woo Kim
--
-- License     : GPL-3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Link where

import           Control.Applicative
import           Control.Concurrent (forkIO)
import           Control.Lens (at,view,set,(%~))
import           Control.Monad (forever,void)
import           Control.Monad.State (get,modify,liftIO,guard,when)
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as B 
import           Data.Foldable (forM_)
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Monoid (mconcat)
import           Data.UUID.V4 (nextRandom)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           DBus
import           DBus.Client
import           Graphics.UI.Gtk hiding (get,set) 
import           System.FilePath 
import           System.Process (createProcess, proc)
-- from hoodle-platform
import           Control.Monad.Trans.Crtn.Queue 
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple
import           Data.Hoodle.Zipper
import           Graphics.Hoodle.Render.Item 
import           Graphics.Hoodle.Render.Type 
import           Graphics.Hoodle.Render.Type.HitTest 
import           Graphics.Hoodle.Render.Util.HitTest 
-- from this package
import           Hoodle.Accessor
import           Hoodle.Coroutine.Dialog
import           Hoodle.Coroutine.Draw
import           Hoodle.Coroutine.Page (changePage)
import           Hoodle.Coroutine.Select.Clipboard
import           Hoodle.Coroutine.TextInput 
import           Hoodle.Coroutine.Window
import           Hoodle.Device 
import           Hoodle.ModelAction.ContextMenu
import           Hoodle.ModelAction.File (makeNewItemImage)
import           Hoodle.ModelAction.Select
import           Hoodle.Type.Canvas
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
import           Hoodle.Type.PageArrangement
import           Hoodle.Util 
import           Hoodle.View.Coordinate
--
import Prelude hiding (mapM_)

-- | 
openLinkAction :: UrlPath 
               -> Maybe (B.ByteString,B.ByteString) -- ^ (docid,anchorid)
               -> MainCoroutine () 
openLinkAction urlpath mid = do 
    liftIO $ putStrLn "openLinkAction"
    liftIO $ print urlpath 
    liftIO $ print mid
    case urlpath of 
      FileUrl fp -> addTab (Just fp)
      HttpUrl url -> do 
        let cmdargs = [url]
        liftIO $ createProcess (proc "xdg-open" cmdargs)  
        return () 

    --       let cmdargs = [url]
    
    --       emit cli (signal "/" "org.ianwookim.hoodle" "findWindow") { signalBody = [ toVariant fp] }         
    --       return () 
    --     HttpUrl url -> do 
    --       let cmdargs = [url]

    -- flip catch (\(ex :: SomeException) -> print ex ) $ do
    --   cli <- connectSession
    --   case urlpath of 
    --     FileUrl fp -> do 
    --       emit cli (signal "/" "org.ianwookim.hoodle" "findWindow") { signalBody = [ toVariant fp] }         
    --       return () 
    --     HttpUrl url -> do 
    --       let cmdargs = [url]
    --       createProcess (proc "xdg-open" cmdargs)  
    --       return () 
    --   forkIO $ do 
    --     threadDelay 2000000
    --     forM_ mid $ \(docid,anchorid) -> do
    --                 print (docid,anchorid)
    --                 emit cli (signal "/" "org.ianwookim.hoodle" "callLink")
    --                            { signalBody = 
    --                                [ toVariant (B.unpack docid 
    --                                             ++ "," 
    --                                             ++ B.unpack anchorid) ] }
    --   return ()

makeTextSVGFromStringAt :: String 
                        -> CanvasId 
                        -> UnitHoodle
                        -> CanvasCoordinate
                        -> IO (B.ByteString, BBox)
makeTextSVGFromStringAt str cid uhdl ccoord = do 
    rdr <- makePangoTextSVG (0,0) (T.pack str) -- for the time being, I use string  
    geometry <- getCanvasGeometryCvsId cid uhdl
    let mpgcoord = (desktop2Page geometry . canvas2Desktop geometry) ccoord 
    return $ case mpgcoord of 
               Nothing -> rdr 
               Just (_,PageCoord (x',y')) -> 
                 let bbox' = moveBBoxULCornerTo (x',y') (snd rdr) 
                 in (fst rdr,bbox')

-- | 
notifyLink :: CanvasId -> PointerCoord -> MainCoroutine () 
notifyLink cid pcoord = do 
    uhdl <- view (unitHoodles.currentUnit) <$> get 
    forBoth' unboxBiAct (f uhdl) (getCanvasInfo cid uhdl)
  where 
    f :: forall b. UnitHoodle -> CanvasInfo b -> MainCoroutine ()
    f uhdl cvsInfo = do 
      let cpn = PageNum . view currentPageNum $ cvsInfo
          arr = view (viewInfo.pageArrangement) cvsInfo              
          mnotifyitem = view notifiedItem cvsInfo
          canvas = view drawArea cvsInfo
      geometry <- liftIO $ makeCanvasGeometry cpn arr canvas
      mresult <- 
        case (desktop2Page geometry . device2Desktop geometry) pcoord of 
          Nothing -> return Nothing 
          Just (pnum,PageCoord (x,y)) -> do 
            let hdl = getHoodle uhdl
                mpage = view (gpages.at (unPageNum pnum)) hdl
            case mpage of
              Nothing -> return Nothing
              Just page -> do 
                let itms =  (view gitems . current . view glayers) page
                    lnks = filter isLinkInRItem itms           
                    hlnks = hltFilteredBy (\itm->isPointInBBox (getBBox itm) (x,y)) lnks
                    hitted = takeHitted hlnks 
                case mnotifyitem of 
                  Nothing -> if ((not.null) hitted) 
                             then Just <$> newNotify geometry pnum (head hitted) Nothing  
                             else return Nothing 
                  Just (opnum,obbx,_) -> do
                    let obbx_desk = xformBBox (unDeskCoord . page2Desktop geometry . (opnum,) . PageCoord) obbx   
                    if pnum == opnum &&  isPointInBBox obbx (x,y) 
                      then return Nothing
                      else if ((not.null) hitted) 
                           then Just <$> newNotify geometry pnum (head hitted) (Just obbx_desk)
                           else return (Just (Nothing,obbx_desk))
      forM_ mresult (\(mnewnotified,bbx_desk) -> do
                      let ncinfobox = (set (unboxLens notifiedItem) mnewnotified . getCanvasInfo cid) uhdl 
                      pureUpdateUhdl $ setCanvasInfo (cid,ncinfobox)
                      invalidateInBBox (Just bbx_desk) Efficient cid )
    ----                                                                                      
    newNotify :: CanvasGeometry -> PageNum -> RItem -> Maybe BBox 
                 -> MainCoroutine (Maybe (PageNum,BBox,RItem),BBox) 
    newNotify geometry pnum lnk mobbx_desk = do        
      let bbx = getBBox lnk
          bbx_desk = xformBBox (unDeskCoord . page2Desktop geometry . (pnum,) . PageCoord) bbx
          nbbx_desk = maybe bbx_desk (unionBBox bbx_desk) mobbx_desk
      return (Just (pnum,bbx,lnk),nbbx_desk)
          

-- | got a link address (or embedded image) from drag and drop             
gotLink :: Maybe String -> (Int,Int) -> MainCoroutine () 
gotLink mstr (x,y) = do 
  xst <- get 
  let uhdl = view (unitHoodles.currentUnit) xst 
      cid = getCurrentCanvasId uhdl
      cache = view renderCache xst
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
      case mr2 of  
        Nothing -> return ()
        Just (FileUrl file) -> do 
          let ext = takeExtension file 
          if ext == ".png" || ext == ".PNG" || ext == ".jpg" || ext == ".JPG" 
            then do 
              let isembedded = view (settings.doesEmbedImage) xst 
              callRenderer $ return . GotRItem =<< cnstrctRItem =<< 
                               liftIO (makeNewItemImage isembedded file)
              RenderEv (GotRItem nitm) <- 
                waitSomeEvent (\case RenderEv (GotRItem _) -> True ; _ -> False )
              geometry <- liftIO $ getCanvasGeometryCvsId cid uhdl               
              let ccoord = CvsCoord (fromIntegral x,fromIntegral y)
                  mpgcoord = (desktop2Page geometry . canvas2Desktop geometry) ccoord 
              insertItemAt mpgcoord nitm 
            else return ()  
        Just (HttpUrl url) -> do 
          case getSelectedItmsFromUnitHoodle uhdl of     
            Nothing -> do 
              uuidbstr <- liftIO $ B.pack . show <$> nextRandom              
              rdrbbx <- liftIO $ makeTextSVGFromStringAt url cid uhdl (CvsCoord (fromIntegral x,fromIntegral y))
              linkInsert "simple" (uuidbstr,url) url rdrbbx
            Just hititms -> do 
              b <- okCancelMessageBox ("replace selected item with link to " ++ url  ++ "?")
              when b $ do 
                let ulbbox = (unUnion . mconcat . fmap (Union . Middle . getBBox)) hititms 
                case ulbbox of 
                  Middle bbox -> do 
                    svg <- liftIO $ makeSVGFromSelection cache hititms bbox
                    uuidbstr <- liftIO $ B.pack . show <$> nextRandom
                    deleteSelection 
                    linkInsert "simple" (uuidbstr,url) url (svg_render svg,bbox)  
                  _ -> return ()          
    Just (uuidbstr,fp) -> do 
      let fn = takeFileName fp 
      case getSelectedItmsFromUnitHoodle uhdl of     
        Nothing -> do 
          rdr <- liftIO (makePangoTextSVG (0,0) (T.pack fn)) 
          geometry <- liftIO $ getCanvasGeometryCvsId cid uhdl 
          let ccoord = CvsCoord (fromIntegral x,fromIntegral y)
              mpgcoord = (desktop2Page geometry . canvas2Desktop geometry) ccoord 
              rdr' = case mpgcoord of 
                       Nothing -> rdr 
                       Just (_,PageCoord (x',y')) -> 
                         let bbox' = moveBBoxULCornerTo (x',y') (snd rdr) in (fst rdr,bbox')
          linkInsert "simple" (uuidbstr,fp) fn rdr' 
        Just hititms -> do 
          b <- okCancelMessageBox ("replace selected item with link to " ++ fn ++ "?")
          when b $ do 
            let ulbbox = (unUnion . mconcat . fmap (Union . Middle . getBBox)) hititms 
            case ulbbox of 
              Middle bbox -> do 
                svg <- liftIO $ makeSVGFromSelection cache hititms bbox
                uuid <- liftIO $ nextRandom
                let uuidbstr' = B.pack (show uuid) 
                deleteSelection 
                linkInsert "simple" (uuidbstr',fp) fn (svg_render svg,bbox)  
              _ -> return ()          
  liftIO $ putStrLn "gotLink"
  liftIO $ print mstr 
  liftIO $ print (x,y)

-- | 
addLink :: MainCoroutine ()
addLink = do 
    mfilename <- fileChooser FileChooserActionOpen Nothing 
    doIOaction $ const (action mfilename) 
    AddLink minput <- waitSomeEvent (\case AddLink _ -> True; _ -> False)
    case minput of 
      Nothing -> return () 
      Just (str,fname) -> do 
        uuid <- liftIO $ nextRandom
        let uuidbstr = B.pack (show uuid)
        rdr <- liftIO (makePangoTextSVG (0,0) (T.pack str)) 
        linkInsert "simple" (uuidbstr,fname) str rdr 
  where 
    action mfn = do  dialog <- messageDialogNew Nothing [DialogModal]
                                 MessageQuestion ButtonsOkCancel ("add link" :: String)
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
                       _ -> widgetDestroy dialog >> return (UsrEv (AddLink Nothing))

                
-- | 
listAnchors :: MainCoroutine ()
listAnchors = liftIO . print . getAnchorMap . rHoodle2Hoodle . getHoodle . view (unitHoodles.currentUnit) =<< get

getAnchorMap :: Hoodle -> M.Map T.Text (Int, (Double,Double))
getAnchorMap hdl = 
    let pgs = view pages hdl
        itemsInPage pg = [i | l <- view layers pg, i <- view items l ]
        anchorsWithPageNum :: [(Int,[Anchor])] 
        anchorsWithPageNum = zip [0..] (map (mapMaybe lookupAnchor . itemsInPage) pgs)
    in foldr (\(p,ys) m -> foldr (insertAnchor p) m ys) M.empty anchorsWithPageNum -- anchormap
  where lookupAnchor (ItemAnchor a) = Just a
        lookupAnchor _ = Nothing
        insertAnchor pgnum (Anchor {..}) = M.insert (TE.decodeUtf8 anchor_id) (pgnum,anchor_pos)  
         
-- | 
startLinkReceiver :: MainCoroutine ()
startLinkReceiver = do 
    callback <- view callBack <$> get 
    liftIO . forkIO $ do
      client <- connectSession
      requestName client "org.ianwookim" []
      forkIO $ void $ addMatch
		 client 
		 matchAny { matchInterface = Just "org.ianwookim.hoodle" 
			  , matchMember = Just "link" } 
	       (goToLink callback)

      forever getLine
    return ()
  where goToLink callback sig = do 
          let txts = mapMaybe fromVariant (signalBody sig) :: [T.Text]
          case txts of 
            txt : _ -> do 
              let  r = T.splitOn "," txt
              case r of 
                docid:anchorid:_ -> 
                  (postGUISync . callback . UsrEv . DBusEv . GoToLink) 
                    (docid,anchorid) 
                _ -> return ()
            _ -> return ()           

goToAnchorPos :: T.Text -> T.Text -> MainCoroutine ()
goToAnchorPos docid anchorid = do 
    rhdl <- getHoodle . view (unitHoodles.currentUnit) <$> get
    let hdl = rHoodle2Hoodle rhdl
    when (docid == (TE.decodeUtf8 . view ghoodleID) rhdl) $ do
      let anchormap = getAnchorMap hdl
      forM_ (M.lookup anchorid anchormap) $ \(pgnum,(x,y))-> do
        liftIO $ print (pgnum,(x,y))
        changePage (const pgnum)

