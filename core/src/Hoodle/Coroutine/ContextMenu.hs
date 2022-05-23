{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hoodle.Coroutine.ContextMenu where

import Control.Applicative
import Control.Concurrent.STM (readTVarIO)
import Control.Lens (set, view, (.~), (^.))
import Control.Monad.State hiding (forM_, mapM_)
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Foldable (forM_, mapM_)
import Data.Hoodle.BBox
import Data.Hoodle.Generic
import Data.Hoodle.Select
import Data.Hoodle.Simple (Anchor (..), Dimension (..), Item (..), Link (..), SVG (..), defaultHoodle)
import qualified Data.Hoodle.Simple as S (Image (..))
import qualified Data.IntMap as IM
import Data.List (partition)
import Data.Monoid
import qualified Data.Text as T (splitAt, unpack)
import qualified Data.Text.Encoding as TE
import Data.UUID.V4
import Graphics.Hoodle.Render
import Graphics.Hoodle.Render.Item
import Graphics.Hoodle.Render.Type
import Graphics.Hoodle.Render.Type.HitTest
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk as Gtk
import Hoodle.Accessor
import Hoodle.Coroutine.Commit
import Hoodle.Coroutine.Dialog
import Hoodle.Coroutine.Draw
import Hoodle.Coroutine.File
import Hoodle.Coroutine.HandwritingRecognition
import Hoodle.Coroutine.Scroll
import Hoodle.Coroutine.Select.Clipboard
import Hoodle.Coroutine.Select.ManipulateImage
import Hoodle.Coroutine.TextInput
import Hoodle.ModelAction.ContextMenu
import Hoodle.ModelAction.Page
import Hoodle.ModelAction.Select
import Hoodle.ModelAction.Select.Transform
import Hoodle.Script.Hook
import Hoodle.Type.Canvas
import Hoodle.Type.Coroutine
import Hoodle.Type.Enum
import Hoodle.Type.Event
import Hoodle.Type.HoodleState
import Hoodle.Type.PageArrangement
import Hoodle.Util
import System.Directory
import System.FilePath
import System.Process
import Text.Hoodle.Builder (builder)
import qualified Text.Hoodlet.Builder as Hoodlet (builder)
--
import Prelude hiding (mapM_)

processContextMenu :: ContextMenuEvent -> MainCoroutine ()
processContextMenu (CMenuSaveSelectionAs ityp) = do
  mhititms <-
    gets (getSelectedItmsFromUnitHoodle . view (unitHoodles . currentUnit))
  forM_ mhititms $ \hititms ->
    let ulbbox = (unUnion . mconcat . fmap (Union . Middle . getBBox)) hititms
     in case ulbbox of
          Middle bbox ->
            case ityp of
              TypSVG -> exportCurrentSelectionAsSVG hititms bbox
              TypPDF -> exportCurrentSelectionAsPDF hititms bbox
          _ -> return ()
processContextMenu CMenuCut = cutSelection
processContextMenu CMenuCopy = copySelection
processContextMenu CMenuDelete = deleteSelection
processContextMenu (CMenuCanvasView cid pnum _x _y) = do
  uhdl <- gets (view (unitHoodles . currentUnit))
  let cmap = uhdl ^. cvsInfoMap
      mcinfobox = IM.lookup cid cmap
  case mcinfobox of
    Nothing -> msgShout "error in processContextMenu"
    Just _cinfobox -> do
      cinfobox' <- liftIO (setPage uhdl pnum cid)
      pureUpdateUhdl (cvsInfoMap .~ IM.adjust (const cinfobox') cid cmap)
      adjustScrollbarWithGeometryCvsId cid
      invalidateAll
processContextMenu (CMenuRotate dir imgbbx) = rotateImage dir imgbbx
processContextMenu (CMenuExport imgbbx) = exportImage (bbxed_content imgbbx)
processContextMenu CMenuAutosavePage = do
  xst <- get
  pg <- getCurrentPageCurr
  mapM_ liftIO $ do
    hset <- xst ^. hookSet
    customAutosavePage hset <*> pure pg
processContextMenu (CMenuLinkConvert nlnk) =
  either_ action . hoodleModeStateEither . (^. hoodleModeState) . view (unitHoodles . currentUnit) =<< get
  where
    action thdl = do
      xst <- get
      let uhdl = view (unitHoodles . currentUnit) xst
          cid = getCurrentCanvasId uhdl
      case thdl ^. gselSelected of
        Nothing -> return ()
        Just (n, tpg) -> do
          let activelayer = rItmsInActiveLyr tpg
              buf = tpg ^. (glayers . selectedLayer . gbuffer)
          ntpg <- case activelayer of
            Left _ -> return tpg
            Right (a :- _b :- as) -> do
              let nitm = ItemLink nlnk
              callRenderer $
                GotRItem <$> cnstrctRItem nitm
              RenderEv (GotRItem nritm) <-
                waitSomeEvent (\case RenderEv (GotRItem _) -> True; _ -> False)
              let alist' = a :- Hitted [nritm] :- as
                  layer' = GLayer buf . TEitherAlterHitted . Right $ alist'
              return (set (glayers . selectedLayer) layer' tpg)
            Right _ -> error "processContextMenu: activelayer"
          nthdl <- updateTempHoodleSelectM cid thdl ntpg n
          uhdl' <- liftIO $ updatePageAll (SelectState nthdl) uhdl
          commit $ (unitHoodles . currentUnit .~ uhdl') xst
          invalidateAll
processContextMenu CMenuCreateALink =
  fileChooser Gtk.FileChooserActionOpen Nothing >>= mapM_ linkSelectionWithFile
processContextMenu CMenuAssocWithNewFile = do
  xst <- get
  let msuggestedact = xst ^. hookSet >>= fileNameSuggestionHook
  (msuggested :: Maybe String) <- maybe (return Nothing) (fmap Just . liftIO) msuggestedact
  fileChooser Gtk.FileChooserActionSave msuggested
    >>= mapM_
      ( \fp -> do
          b <- liftIO (doesFileExist fp)
          if b
            then okMessageBox "The file already exist!"
            else do
              doIOaction_ $ do
                nhdl <- liftIO defaultHoodle
                (L.writeFile fp . builder) nhdl
                createProcess (proc "hoodle" [fp])
              linkSelectionWithFile fp
              return ()
      )
processContextMenu (CMenuMakeLinkToAnchor anc) = do
  xst <- get
  uuidbstr <- liftIO $ B.pack . show <$> nextRandom
  let uhdl = view (unitHoodles . currentUnit) xst
      docidbstr = (view ghoodleID . getHoodle) uhdl
      mloc = case view (hoodleFileControl . hoodleFileName) uhdl of
        LocalDir Nothing -> Nothing
        LocalDir (Just fp) -> Just fp
        TempDir _ -> Nothing
      loc = maybe "" B.pack mloc
      lnk = LinkAnchor uuidbstr docidbstr loc (anchor_id anc) "" (0, 0) (Dim 50 50)
  callRenderer $
    GotRItem <$> cnstrctRItem (ItemLink lnk)
  RenderEv (GotRItem newitem) <-
    waitSomeEvent (\case RenderEv (GotRItem _) -> True; _ -> False)
  insertItemAt Nothing newitem
processContextMenu (CMenuPangoConvert (x0, y0) txt) = textInput (Just (x0, y0)) txt
processContextMenu (CMenuLaTeXConvert (x0, y0) txt) = laTeXInput (Just (x0, y0)) txt
processContextMenu (CMenuLaTeXConvertNetwork (x0, y0) txt) = laTeXInputNetwork (Just (x0, y0)) txt
processContextMenu (CMenuLaTeXUpdate (x0, y0) dim key) =
  void $ runMaybeT (laTeXInputKeyword (x0, y0) (Just dim) key)
processContextMenu (CMenuCropImage imgbbox) = cropImage imgbbox
processContextMenu (CMenuExportHoodlet itm) = do
  res <- handwritingRecognitionDialog
  forM_ res $ \(b, txt) -> do
    unless b $
      liftIO $ do
        let str = T.unpack txt
        homedir <- getHomeDirectory
        let hoodled = homedir </> ".hoodle.d"
            hoodletdir = hoodled </> "hoodlet"
        b' <- doesDirectoryExist hoodletdir
        unless b' $
          createDirectory hoodletdir
        let fp = hoodletdir </> str <.> "hdlt"
        L.writeFile fp (Hoodlet.builder itm)
processContextMenu (CMenuConvertSelection itm) = do
  uhdl <- gets (view (unitHoodles . currentUnit))
  let pgnum = view (currentCanvasInfo . unboxLens currentPageNum) uhdl
  deleteSelection
  callRenderer $
    GotRItem <$> cnstrctRItem itm
  RenderEv (GotRItem newitem) <- waitSomeEvent (\case RenderEv (GotRItem _) -> True; _ -> False)
  let BBox (x0, y0) _ = getBBox newitem
  insertItemAt (Just (PageNum pgnum, PageCoord (x0, y0))) newitem
processContextMenu CMenuCustom = do
  either_ action . hoodleModeStateEither . view hoodleModeState . view (unitHoodles . currentUnit) =<< get
  where
    action thdl = do
      xst <- get
      forM_
        (view gselSelected thdl)
        ( \(_, tpg) -> do
            let hititms = (map rItem2Item . getSelectedItms) tpg
            mapM_ liftIO $ do
              hset <- view hookSet xst
              customContextMenuHook hset <*> pure hititms
        )

--  |
linkSelectionWithFile :: FilePath -> MainCoroutine ()
linkSelectionWithFile fname = do
  uhdl <- gets (view (unitHoodles . currentUnit))
  let cid = getCurrentCanvasId uhdl
  forM_ (getSelectedItmsFromUnitHoodle uhdl) $ \hititms ->
    let ulbbox = (unUnion . mconcat . fmap (Union . Middle . getBBox)) hititms
     in case ulbbox of
          Middle bbox -> do
            cache <- renderCache
            svg <- liftIO $ makeSVGFromSelection cache cid hititms bbox
            uuid <- liftIO nextRandom
            let uuidbstr = B.pack (show uuid)
            deleteSelection
            linkInsert "simple" (uuidbstr, fname) fname (svg_render svg, bbox)
          _ -> return ()

-- |
exportCurrentSelectionAsSVG :: [RItem] -> BBox -> MainCoroutine ()
exportCurrentSelectionAsSVG hititms bbox@(BBox (ulx, uly) (lrx, lry)) =
  fileChooser Gtk.FileChooserActionSave Nothing >>= mapM_ action
  where
    action filename = do
      (cache, cid) <-
        (,)
          <$> renderCache
          <*> gets (getCurrentCanvasId . view (unitHoodles . currentUnit))
      -- this is rather temporary not to make mistake
      if takeExtension filename /= ".svg"
        then
          fileExtensionInvalid (".svg", "export")
            >> exportCurrentSelectionAsSVG hititms bbox
        else do
          liftIO $
            Cairo.withSVGSurface filename (lrx - ulx) (lry - uly) $ \s ->
              Cairo.renderWith s $ do
                Cairo.translate (-ulx) (-uly)
                mapM_ (renderRItem cache cid) hititms

exportCurrentSelectionAsPDF :: [RItem] -> BBox -> MainCoroutine ()
exportCurrentSelectionAsPDF hititms bbox@(BBox (ulx, uly) (lrx, lry)) =
  fileChooser Gtk.FileChooserActionSave Nothing >>= mapM_ action
  where
    action filename = do
      (cache, cid) <-
        (,)
          <$> renderCache
          <*> gets (getCurrentCanvasId . view (unitHoodles . currentUnit))
      -- this is rather temporary not to make mistake
      if takeExtension filename /= ".pdf"
        then
          fileExtensionInvalid (".svg", "export")
            >> exportCurrentSelectionAsPDF hititms bbox
        else do
          liftIO $
            Cairo.withPDFSurface filename (lrx - ulx) (lry - uly) $ \s ->
              Cairo.renderWith s $ do
                Cairo.translate (-ulx) (-uly)
                mapM_ (renderRItem cache cid) hititms

-- |
exportImage :: S.Image -> MainCoroutine ()
exportImage img =
  void $
    runMaybeT $ do
      pngbstr <- (MaybeT . return . getByteStringIfEmbeddedPNG . S.img_src) img
      fp <- MaybeT (fileChooser Gtk.FileChooserActionSave Nothing)
      liftIO $ B.writeFile fp pngbstr

showContextMenu :: (PageNum, (Double, Double)) -> MainCoroutine ()
showContextMenu (pnum, (x, y)) = do
  xstate <- get
  when (view (settings . doesUsePopUpMenu) xstate) $
    do
      let uhdl = view (unitHoodles . currentUnit) xstate
          cids = IM.keys . view cvsInfoMap $ uhdl
          cid = fst . view currentCanvas $ uhdl
          mselitms = do
            lst <- getSelectedItmsFromUnitHoodle uhdl
            if null lst then Nothing else Just lst
      doIOaction (action xstate mselitms cid cids)
      >> waitSomeEvent (\case ContextMenuCreated -> True; _ -> False)
      >> return ()
  where
    action xstate msitms cid cids evhandler = do
      menu <- Gtk.menuNew
      Gtk.menuSetTitle menu ("MyMenu" :: String)
      case msitms of
        Nothing -> return ()
        Just sitms -> do
          menuitem1 <- Gtk.menuItemNewWithLabel ("Make SVG" :: String)
          menuitem2 <- Gtk.menuItemNewWithLabel ("Make PDF" :: String)
          menuitem3 <- Gtk.menuItemNewWithLabel ("Cut" :: String)
          menuitem4 <- Gtk.menuItemNewWithLabel ("Copy" :: String)
          menuitem5 <- Gtk.menuItemNewWithLabel ("Delete" :: String)
          menuitem6 <- Gtk.menuItemNewWithLabel ("New File Linked Here" :: String)
          _ <-
            menuitem1 `Gtk.on` Gtk.menuItemActivate $
              evhandler (UsrEv (GotContextMenuSignal (CMenuSaveSelectionAs TypSVG)))
          _ <-
            menuitem2 `Gtk.on` Gtk.menuItemActivate $
              evhandler (UsrEv (GotContextMenuSignal (CMenuSaveSelectionAs TypPDF)))
          _ <-
            menuitem3 `Gtk.on` Gtk.menuItemActivate $
              evhandler (UsrEv (GotContextMenuSignal CMenuCut))
          _ <-
            menuitem4 `Gtk.on` Gtk.menuItemActivate $
              evhandler (UsrEv (GotContextMenuSignal CMenuCopy))
          _ <-
            menuitem5 `Gtk.on` Gtk.menuItemActivate $
              evhandler (UsrEv (GotContextMenuSignal CMenuDelete))
          _ <-
            menuitem6 `Gtk.on` Gtk.menuItemActivate $
              evhandler (UsrEv (GotContextMenuSignal CMenuAssocWithNewFile))
          Gtk.menuAttach menu menuitem1 0 1 1 2
          Gtk.menuAttach menu menuitem2 0 1 2 3
          Gtk.menuAttach menu menuitem3 1 2 0 1
          Gtk.menuAttach menu menuitem4 1 2 1 2
          Gtk.menuAttach menu menuitem5 1 2 2 3
          Gtk.menuAttach menu menuitem6 1 2 3 4
          mapM_ (\mi -> Gtk.menuAttach menu mi 1 2 5 6) =<< menuCreateALink evhandler sitms
          case sitms of
            [sitm] -> do
              menuhdlt <- Gtk.menuItemNewWithLabel ("Make Hoodlet" :: String)
              _ <-
                menuhdlt `Gtk.on` Gtk.menuItemActivate $
                  ( evhandler . UsrEv . GotContextMenuSignal
                      . CMenuExportHoodlet
                      . rItem2Item
                  )
                    sitm
              Gtk.menuAttach menu menuhdlt 0 1 8 9
              case sitm of
                RItemLink lnkbbx _msfc -> do
                  let lnk = bbxed_content lnkbbx
                  forM_
                    ((urlParse . B.unpack . link_location) lnk)
                    ( \urlpath -> do
                        milnk <- menuOpenALink evhandler urlpath
                        Gtk.menuAttach menu milnk 0 1 3 4
                    )
                  case lnk of
                    Link _i _typ _lstr _txt _cmd _rdr _pos _dim ->
                      convertLinkFromSimpleToDocID lnk
                        >>= mapM_
                          ( \link -> do
                              let LinkDocID _ uuid _ _ _ _ _ _ = link
                              menuitemcvt <- Gtk.menuItemNewWithLabel ("Convert Link With ID" ++ show uuid :: String)
                              _ <-
                                menuitemcvt `Gtk.on` Gtk.menuItemActivate $
                                  ( evhandler
                                      . UsrEv
                                      . GotContextMenuSignal
                                      . CMenuLinkConvert
                                  )
                                    link
                              Gtk.menuAttach menu menuitemcvt 0 1 4 5
                          )
                    LinkDocID i lid file txt cmd rdr pos dim ->
                      void $
                        runMaybeT $ do
                          hset <- (MaybeT . return . view hookSet) xstate
                          f <- (MaybeT . return . lookupPathFromId) hset
                          file' <- MaybeT (f (B.unpack lid))
                          guard (B.unpack file /= file')
                          let link =
                                LinkDocID
                                  i
                                  lid
                                  (B.pack file')
                                  txt
                                  cmd
                                  rdr
                                  pos
                                  dim
                          menuitemcvt <-
                            liftIO $
                              Gtk.menuItemNewWithLabel
                                ("Correct Path to " ++ show file' :: String)
                          _ <-
                            liftIO
                              ( menuitemcvt `Gtk.on` Gtk.menuItemActivate $
                                  ( evhandler
                                      . UsrEv
                                      . GotContextMenuSignal
                                      . CMenuLinkConvert
                                  )
                                    link
                              )
                          liftIO $ Gtk.menuAttach menu menuitemcvt 0 1 4 5
                    LinkAnchor i lid file aid bstr pos dim ->
                      void $
                        runMaybeT $ do
                          hset <- (MaybeT . return . view hookSet) xstate
                          f <- (MaybeT . return . lookupPathFromId) hset
                          file' <- MaybeT (f (B.unpack lid))
                          guard (B.unpack file /= file')
                          let link = LinkAnchor i lid (B.pack file') aid bstr pos dim
                          menuitemcvt <-
                            liftIO $
                              Gtk.menuItemNewWithLabel
                                ("Correct Path to " ++ show file' :: String)
                          void $
                            liftIO
                              ( menuitemcvt `Gtk.on` Gtk.menuItemActivate $
                                  ( evhandler
                                      . UsrEv
                                      . GotContextMenuSignal
                                      . CMenuLinkConvert
                                  )
                                    link
                              )
                          liftIO $ Gtk.menuAttach menu menuitemcvt 0 1 4 5
                RItemSVG svgbbx _msfc -> do
                  let svg = bbxed_content svgbbx
                      BBox (x0, y0) (x1, y1) = getBBox svgbbx
                  forM_ ((,) <$> svg_text svg <*> svg_command svg) $ \(btxt, cmd) -> do
                    let txt = TE.decodeUtf8 btxt
                    case cmd of
                      "pango" -> do
                        menuitemedt <- Gtk.menuItemNewWithLabel ("Edit Text" :: String)
                        _ <- menuitemedt `Gtk.on` Gtk.menuItemActivate $ do
                          evhandler (UsrEv (GotContextMenuSignal (CMenuPangoConvert (x0, y0) txt)))
                        Gtk.menuAttach menu menuitemedt 0 1 4 5
                        return ()
                      "latex" -> do
                        menuitemedt <- Gtk.menuItemNewWithLabel ("Edit LaTeX" :: String)
                        _ <- menuitemedt `Gtk.on` Gtk.menuItemActivate $ do
                          evhandler (UsrEv (GotContextMenuSignal (CMenuLaTeXConvert (x0, y0) txt)))
                        Gtk.menuAttach menu menuitemedt 0 1 4 5
                        --
                        menuitemnet <- Gtk.menuItemNewWithLabel ("Edit LaTeX Network" :: String)
                        _ <- menuitemnet `Gtk.on` Gtk.menuItemActivate $ do
                          evhandler (UsrEv (GotContextMenuSignal (CMenuLaTeXConvertNetwork (x0, y0) txt)))
                        Gtk.menuAttach menu menuitemnet 0 1 5 6
                        --
                        let (txth, txtt) = T.splitAt 19 txt
                        when (txth == "embedlatex:keyword:") $ do
                          menuitemup <- Gtk.menuItemNewWithLabel ("Update LaTeX" :: String)
                          _ <- menuitemup `Gtk.on` Gtk.menuItemActivate $ do
                            evhandler (UsrEv (GotContextMenuSignal (CMenuLaTeXUpdate (x0, y0) (Dim (x1 - x0) (y1 - y0)) txtt)))
                          Gtk.menuAttach menu menuitemup 0 1 6 7
                          return ()
                      _ -> return ()
                RItemImage imgbbx _msfc -> do
                  menuitemcrop <- Gtk.menuItemNewWithLabel ("Crop Image" :: String)
                  _ <- menuitemcrop `Gtk.on` Gtk.menuItemActivate $ do
                    (evhandler . UsrEv . GotContextMenuSignal . CMenuCropImage) imgbbx
                  menuitemrotcw <- Gtk.menuItemNewWithLabel ("Rotate Image CW" :: String)
                  _ <- menuitemrotcw `Gtk.on` Gtk.menuItemActivate $ do
                    (evhandler . UsrEv . GotContextMenuSignal) (CMenuRotate CW imgbbx)
                  menuitemrotccw <- Gtk.menuItemNewWithLabel ("Rotate Image CCW" :: String)
                  _ <- menuitemrotccw `Gtk.on` Gtk.menuItemActivate $ do
                    (evhandler . UsrEv . GotContextMenuSignal) (CMenuRotate CCW imgbbx)
                  menuitemexport <- Gtk.menuItemNewWithLabel ("Export Image" :: String)
                  _ <- menuitemexport `Gtk.on` Gtk.menuItemActivate $ do
                    (evhandler . UsrEv . GotContextMenuSignal) (CMenuExport imgbbx)
                  --
                  Gtk.menuAttach menu menuitemcrop 0 1 4 5
                  Gtk.menuAttach menu menuitemrotcw 0 1 5 6
                  Gtk.menuAttach menu menuitemrotccw 0 1 6 7
                  Gtk.menuAttach menu menuitemexport 0 1 7 8
                  --
                  return ()
                RItemAnchor ancbbx _ -> do
                  menuitemmklnk <- Gtk.menuItemNewWithLabel ("Link to this anchor" :: String)
                  _ <-
                    menuitemmklnk `Gtk.on` Gtk.menuItemActivate $
                      ( evhandler
                          . UsrEv
                          . GotContextMenuSignal
                          . CMenuMakeLinkToAnchor
                          . bbxed_content
                      )
                        ancbbx
                  Gtk.menuAttach menu menuitemmklnk 0 1 4 5
                _ -> return ()
            _ -> do
              let (links, others) = partition ((||) <$> isLinkInRItem <*> isAnchorInRItem) sitms
              case links of
                [l] -> do
                  menuitemreplace <- Gtk.menuItemNewWithLabel ("replace link/anchor render" :: String)
                  _ <- menuitemreplace `Gtk.on` Gtk.menuItemActivate $ do
                    cache <- readTVarIO (xstate ^. renderCacheVar)
                    let ulbbox = (unUnion . mconcat . fmap (Union . Middle . getBBox)) others
                    case ulbbox of
                      Middle bbox -> do
                        let BBox (x0, y0) (x1, y1) = bbox
                            dim = Dim (x1 - x0) (y1 - y0)
                        svg <- svg_render <$> makeSVGFromSelection cache cid others bbox
                        let mitm = case l of
                              RItemLink lnkbbx _ -> (Just . ItemLink) ((bbxed_content lnkbbx) {link_render = svg, link_dim = dim})
                              RItemAnchor ancbbx _ -> (Just . ItemAnchor) ((bbxed_content ancbbx) {anchor_render = svg, anchor_dim = dim})
                              _ -> Nothing
                        maybe (return ()) (evhandler . UsrEv . GotContextMenuSignal . CMenuConvertSelection) mitm
                      _ -> return ()
                  Gtk.menuAttach menu menuitemreplace 0 1 8 9
                _ -> return ()
      case customContextMenuTitle =<< view hookSet xstate of
        Nothing -> return ()
        Just ttl -> do
          custommenu <- Gtk.menuItemNewWithLabel ttl
          _ <-
            custommenu `Gtk.on` Gtk.menuItemActivate $
              evhandler (UsrEv (GotContextMenuSignal CMenuCustom))
          Gtk.menuAttach menu custommenu 0 1 0 1
      menuitem8 <- Gtk.menuItemNewWithLabel ("Autosave This Page Image" :: String)
      _ <-
        menuitem8 `Gtk.on` Gtk.menuItemActivate $
          evhandler (UsrEv (GotContextMenuSignal CMenuAutosavePage))
      Gtk.menuAttach menu menuitem8 1 2 4 5
      _ <- runStateT (mapM_ (makeMenu evhandler menu cid) cids) 0
      Gtk.widgetShowAll menu
      Gtk.menuPopup menu Nothing
      return (UsrEv ContextMenuCreated)
    makeMenu evhdlr mn currcid cid = when (currcid /= cid) $ do
      n <- get
      mi <- liftIO $ Gtk.menuItemNewWithLabel ("Show here in cvs" ++ show cid)
      _ <-
        liftIO $
          Gtk.on mi Gtk.menuItemActivate $
            evhdlr (UsrEv (GotContextMenuSignal (CMenuCanvasView cid pnum x y)))
      liftIO $ Gtk.menuAttach mn mi 2 3 n (n + 1)
      put (n + 1)
