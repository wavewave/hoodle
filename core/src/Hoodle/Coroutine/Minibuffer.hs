{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Hoodle.Coroutine.Minibuffer where

import Control.Applicative ((<$>), (<*>))
import Control.Lens (view)
import Control.Monad.State (get)
import Control.Monad.Trans (liftIO)
import Data.Foldable (Foldable (..), forM_, mapM_, toList)
--
import Data.Hoodle.Simple
import Data.Sequence (Seq, ViewL (..), empty, singleton, viewl, (|>))
import Graphics.Hoodle.Render (renderStrk)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.Gtk as Gtk
--
import Hoodle.Coroutine.Draw
import Hoodle.Device
import Hoodle.ModelAction.Pen (createNewStroke)
import Hoodle.Type.Canvas (defaultPenInfo, defaultPenWCS, penWidth)
import Hoodle.Type.Coroutine
import Hoodle.Type.Enum
import Hoodle.Type.Event
import Hoodle.Type.HoodleState
--
import Prelude hiding (length, mapM_)

drawMiniBufBkg :: Cairo.Render ()
drawMiniBufBkg = do
  Cairo.setSourceRGBA 0.8 0.8 0.8 1
  Cairo.rectangle 0 0 500 50
  Cairo.fill
  Cairo.setSourceRGBA 0.95 0.85 0.5 1
  Cairo.rectangle 5 2 490 46
  Cairo.fill
  Cairo.setSourceRGBA 0 0 0 1
  Cairo.setLineWidth 1.0
  Cairo.rectangle 5 2 490 46
  Cairo.stroke

drawMiniBuf :: (Foldable t) => t Stroke -> Cairo.Render ()
drawMiniBuf strks = drawMiniBufBkg >> mapM_ renderStrk strks

minibufDialog :: String -> MainCoroutine (Either () [Stroke])
minibufDialog msg = do
  xst <- get
  let dev = view deviceList xst
  let ui = view gtkUIManager xst
  agr <-
    liftIO
      ( Gtk.uiManagerGetActionGroups ui >>= \x ->
          case x of
            [] -> error "No action group? "
            y : _ -> return y
      )
  uxinputa <-
    liftIO
      ( Gtk.actionGroupGetAction agr "UXINPUTA" >>= \(Just x) ->
          return (Gtk.castToToggleAction x)
      )
  doesUseX11Ext <- liftIO $ Gtk.toggleActionGetActive uxinputa
  doIOaction (action dev doesUseX11Ext)
  minibufInit
  where
    action dev _doesUseX11Ext = \evhandler -> do
      dialog <- Gtk.dialogNew
      msgLabel <- Gtk.labelNew (Just msg)
      cvs <- Gtk.drawingAreaNew
      Gtk.widgetSetSizeRequest cvs 500 50
      cvs `Gtk.on` Gtk.draw $ do
        -- TODO: purify this
        drawwdw <-
          liftIO $
            Gtk.widgetGetWindow cvs >>= \case
              Just drawwdw -> pure drawwdw
              _ -> error "minibufDialog: now canvas window"
        liftIO (Gtk.renderWithDrawWindow drawwdw drawMiniBufBkg)
        (liftIO . evhandler . UsrEv . MiniBuffer . MiniBufferInitialized) drawwdw
      cvs `Gtk.on` Gtk.buttonPressEvent $
        Gtk.tryEvent $ do
          (mbtn, mp) <- getPointer dev
          forM_ mp $ \p -> do
            let pbtn = maybe PenButton1 id mbtn
            case pbtn of
              TouchButton -> return ()
              _ -> (liftIO . evhandler . UsrEv . MiniBuffer) (MiniBufferPenDown pbtn p)
      cvs `Gtk.on` Gtk.buttonReleaseEvent $
        Gtk.tryEvent $ do
          (mbtn, mp) <- getPointer dev
          forM_ mp $ \p -> do
            let pbtn = maybe PenButton1 id mbtn
            case pbtn of
              TouchButton -> return ()
              _ -> (liftIO . evhandler . UsrEv . MiniBuffer) (MiniBufferPenUp p)
      cvs `Gtk.on` Gtk.motionNotifyEvent $
        Gtk.tryEvent $ do
          (mbtn, mp) <- getPointer dev
          forM_ mp $ \p -> do
            let pbtn = maybe PenButton1 id mbtn
            case pbtn of
              TouchButton -> return ()
              _ -> (liftIO . evhandler . UsrEv . MiniBuffer) (MiniBufferPenMove p)
      Gtk.widgetAddEvents cvs [Gtk.PointerMotionMask, Gtk.Button1MotionMask]
      upper <- fmap Gtk.castToContainer (Gtk.dialogGetContentArea dialog)
      vbox <- Gtk.vBoxNew False 0
      Gtk.containerAdd upper vbox
      hbox <- Gtk.hBoxNew False 0
      Gtk.boxPackStart hbox msgLabel Gtk.PackGrow 0
      Gtk.boxPackStart vbox hbox Gtk.PackGrow 0
      Gtk.boxPackStart vbox cvs Gtk.PackNatural 0
      _btnOk <- Gtk.dialogAddButton dialog "Ok" Gtk.ResponseOk
      _btnCancel <- Gtk.dialogAddButton dialog "Cancel" Gtk.ResponseCancel
      _btnText <- Gtk.dialogAddButton dialog "TextInput" (Gtk.ResponseUser 1)
      Gtk.widgetShowAll dialog
      res <- Gtk.dialogRun dialog
      Gtk.widgetDestroy dialog
      case res of
        Gtk.ResponseOk -> return (UsrEv (OkCancel True))
        Gtk.ResponseCancel -> return (UsrEv (OkCancel False))
        Gtk.ResponseUser 1 -> return (UsrEv ChangeDialog)
        _ -> return (UsrEv (OkCancel False))

minibufInit :: MainCoroutine (Either () [Stroke])
minibufInit =
  waitSomeEvent (\case MiniBuffer (MiniBufferInitialized _) -> True; _ -> False)
    >>= ( \case
            MiniBuffer (MiniBufferInitialized drawwdw) -> do
              srcsfc <-
                liftIO
                  ( Cairo.createImageSurface
                      Cairo.FormatARGB32
                      500
                      50
                  )
              tgtsfc <-
                liftIO
                  ( Cairo.createImageSurface
                      Cairo.FormatARGB32
                      500
                      50
                  )
              liftIO $ Cairo.renderWith srcsfc (drawMiniBuf empty)
              liftIO $ invalidateMinibuf drawwdw srcsfc
              minibufStart drawwdw (srcsfc, tgtsfc) empty
            _ -> minibufInit
        )

invalidateMinibuf :: Gtk.DrawWindow -> Cairo.Surface -> IO ()
invalidateMinibuf drawwdw tgtsfc =
  Gtk.renderWithDrawWindow drawwdw $ do
    Cairo.setSourceSurface tgtsfc 0 0
    Cairo.setOperator Cairo.OperatorSource
    Cairo.paint

minibufStart ::
  Gtk.DrawWindow ->
  -- | (source, target)
  (Cairo.Surface, Cairo.Surface) ->
  Seq Stroke ->
  MainCoroutine (Either () [Stroke])
minibufStart drawwdw (srcsfc, tgtsfc) strks = do
  r <- nextevent
  case r of
    UpdateCanvas cid -> do
      invalidateInBBox Nothing Efficient cid
      minibufStart drawwdw (srcsfc, tgtsfc) strks
    OkCancel True -> (return . Right) (toList strks)
    OkCancel False -> (return . Right) []
    ChangeDialog -> return (Left ())
    MiniBuffer (MiniBufferPenDown PenButton1 pcoord) -> do
      ps <- onestroke drawwdw (srcsfc, tgtsfc) (singleton pcoord)
      let nstrks = strks |> mkstroke ps
      liftIO $ Cairo.renderWith srcsfc (drawMiniBuf nstrks)
      minibufStart drawwdw (srcsfc, tgtsfc) nstrks
    _ -> minibufStart drawwdw (srcsfc, tgtsfc) strks

onestroke ::
  Gtk.DrawWindow ->
  -- | (source, target)
  (Cairo.Surface, Cairo.Surface) ->
  Seq PointerCoord ->
  MainCoroutine (Seq PointerCoord)
onestroke drawwdw (srcsfc, tgtsfc) pcoords = do
  r <- nextevent
  case r of
    MiniBuffer (MiniBufferPenMove pcoord) -> do
      let newpcoords = pcoords |> pcoord
      liftIO $ do
        drawstrokebit (srcsfc, tgtsfc) newpcoords
        invalidateMinibuf drawwdw tgtsfc
      onestroke drawwdw (srcsfc, tgtsfc) newpcoords
    MiniBuffer (MiniBufferPenUp pcoord) -> return (pcoords |> pcoord)
    _ -> onestroke drawwdw (srcsfc, tgtsfc) pcoords

drawstrokebit ::
  (Cairo.Surface, Cairo.Surface) ->
  Seq PointerCoord ->
  IO ()
drawstrokebit (srcsfc, tgtsfc) ps =
  Cairo.renderWith tgtsfc $ do
    Cairo.setSourceSurface srcsfc 0 0
    Cairo.setOperator Cairo.OperatorSource
    Cairo.paint
    case viewl ps of
      p :< ps' -> do
        Cairo.setOperator Cairo.OperatorOver
        Cairo.setSourceRGBA 0.0 0.0 0.0 1.0
        Cairo.setLineWidth (view penWidth defaultPenWCS)
        Cairo.moveTo (pointerX p) (pointerY p)
        mapM_ (uncurry Cairo.lineTo . ((,) <$> pointerX <*> pointerY)) ps'
        Cairo.stroke
      _ -> return ()

mkstroke :: Seq PointerCoord -> Stroke
mkstroke ps =
  let xyzs = fmap ((,,) <$> pointerX <*> pointerY <*> const 1.0) ps
      pinfo = defaultPenInfo
   in createNewStroke pinfo xyzs
