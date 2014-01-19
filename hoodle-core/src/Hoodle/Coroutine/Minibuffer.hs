{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Minibuffer 
-- Copyright   : (c) 2013, 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Minibuffer where 

import           Control.Applicative ((<$>),(<*>))
import           Control.Lens ((%~),view)
import           Control.Monad.State (modify,get)
import           Data.Foldable (Foldable(..),mapM_,forM_,toList)
import           Data.Sequence (Seq,(|>),empty,singleton,viewl,ViewL(..))
import           Graphics.UI.Gtk hiding (get,set)
import           Graphics.Rendering.Cairo
-- 
import           Control.Monad.Trans.Crtn.Queue (enqueue)
import           Data.Hoodle.Simple
import           Graphics.Hoodle.Render (renderStrk)
--
import           Hoodle.Coroutine.Draw
import           Hoodle.Device
import           Hoodle.ModelAction.Pen (createNewStroke)
import           Hoodle.Type.Canvas (defaultPenInfo, defaultPenWCS, penWidth)
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Enum
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
--
import           Prelude hiding (length,mapM_)

drawMiniBufBkg :: Render ()
drawMiniBufBkg = do           
    setSourceRGBA 0.8 0.8 0.8 1 
    rectangle 0 0 500 50
    fill
    setSourceRGBA 0.95 0.85 0.5 1
    rectangle 5 2 490 46
    fill 
    setSourceRGBA 0 0 0 1
    setLineWidth 1.0
    rectangle 5 2 490 46 
    stroke

drawMiniBuf :: (Foldable t) => t Stroke -> Render ()
drawMiniBuf strks = drawMiniBufBkg >> mapM_ renderStrk strks
    

minibufDialog :: String -> MainCoroutine (Either () [Stroke])
minibufDialog msg = do 
    xst <- get
    let dev = view deviceList xst 
    let ui = view gtkUIManager xst 
    agr <- liftIO ( uiManagerGetActionGroups ui >>= \x ->
                      case x of 
                        [] -> error "No action group? "
                        y:_ -> return y )
    uxinputa <- liftIO (actionGroupGetAction agr "UXINPUTA" >>= \(Just x) -> 
                          return (castToToggleAction x) )
    doesUseX11Ext <- liftIO $ toggleActionGetActive uxinputa
    modify (tempQueue %~ enqueue (action dev doesUseX11Ext)) 
    minibufInit
  where 
    action dev _doesUseX11Ext = mkIOaction $ \evhandler -> do 
      dialog <- dialogNew 
      msgLabel <- labelNew (Just msg) 
      cvs <- drawingAreaNew                           
      cvs `on` sizeRequest $ return (Requisition 500 50)
      cvs `on` exposeEvent $ tryEvent $ do
#ifdef GTK3        
        Just drawwdw <- liftIO $ widgetGetWindow cvs
#else // GTK3
        drawwdw <- liftIO $ widgetGetDrawWindow cvs
#endif // GTK3
#ifdef GTK3
        liftIO (renderWithDrawWindow drawwdw drawMiniBufBkg)
#else // GTK3
        liftIO (renderWithDrawable drawwdw drawMiniBufBkg)
#endif // GTK3
        (liftIO . evhandler . UsrEv . MiniBuffer . MiniBufferInitialized) drawwdw
      cvs `on` buttonPressEvent $ tryEvent $ do 
        (mbtn,mp) <- getPointer dev
        forM_ mp $ \p -> do
          let pbtn = maybe PenButton1 id mbtn 
          case pbtn of
            TouchButton -> return ()
            _ -> (liftIO . evhandler . UsrEv . MiniBuffer) (MiniBufferPenDown pbtn p)
      cvs `on` buttonReleaseEvent $ tryEvent $ do 
        (mbtn,mp) <- getPointer dev
        forM_ mp $ \p -> do  
          let pbtn = maybe PenButton1 id mbtn 
          case pbtn of
            TouchButton -> return ()
            _ -> (liftIO . evhandler . UsrEv . MiniBuffer) (MiniBufferPenUp p)
      cvs `on` motionNotifyEvent $ tryEvent $ do 
        (mbtn,mp) <- getPointer dev
        forM_ mp $ \p -> do  
            let pbtn = maybe PenButton1 id mbtn      
            case pbtn of 
              TouchButton -> return () 
              _ -> (liftIO . evhandler . UsrEv . MiniBuffer) (MiniBufferPenMove p)
      {- if doesUseX11Ext 
        then widgetSetExtensionEvents cvs [ExtensionEventsAll]
        else widgetSetExtensionEvents cvs [ExtensionEventsNone] -}
      widgetAddEvents cvs [PointerMotionMask,Button1MotionMask]      
      --
#ifdef GTK3
      upper <- fmap castToContainer (dialogGetContentArea dialog)
      vbox <- vBoxNew False 0 
      containerAdd upper vbox
#else // GTK3
      vbox <- dialogGetUpper dialog
#endif // GTK3
      hbox <- hBoxNew False 0 
      boxPackStart hbox msgLabel PackNatural 0 
      boxPackStart vbox hbox PackNatural 0
      boxPackStart vbox cvs PackNatural 0
      _btnOk <- dialogAddButton dialog "Ok" ResponseOk
      _btnCancel <- dialogAddButton dialog "Cancel" ResponseCancel
      _btnText <- dialogAddButton dialog "TextInput" (ResponseUser 1) 
      widgetShowAll dialog
      res <- dialogRun dialog 
      widgetDestroy dialog 
      case res of 
        ResponseOk -> return (UsrEv (OkCancel True))
        ResponseCancel -> return (UsrEv (OkCancel False))
        ResponseUser 1 -> return (UsrEv ChangeDialog)
        _ -> return (UsrEv (OkCancel False))

minibufInit :: MainCoroutine (Either () [Stroke])
minibufInit = 
  waitSomeEvent (\case MiniBuffer (MiniBufferInitialized _ )-> True ; _ -> False) 
  >>= (\case MiniBuffer (MiniBufferInitialized drawwdw) -> do
               srcsfc <- liftIO (createImageSurface FormatARGB32 500 50)
               tgtsfc <- liftIO (createImageSurface FormatARGB32 500 50)
               liftIO $ renderWith srcsfc (drawMiniBuf empty) 
               liftIO $ invalidateMinibuf drawwdw srcsfc 
               minibufStart drawwdw (srcsfc,tgtsfc) empty 
             _ -> minibufInit)

invalidateMinibuf :: DrawWindow -> Surface -> IO ()
invalidateMinibuf drawwdw tgtsfc = 
#ifdef GTK3
  renderWithDrawWindow drawwdw $ do 
#else // GTK3
  renderWithDrawable drawwdw $ do 
#endif // GTK3
    setSourceSurface tgtsfc 0 0 
    setOperator OperatorSource 
    paint   

minibufStart :: DrawWindow 
             -> (Surface,Surface)  -- ^ (source surface, target surface)
             -> Seq Stroke -> MainCoroutine (Either () [Stroke])
minibufStart drawwdw (srcsfc,tgtsfc) strks = do 
    r <- nextevent 
    case r of 
      UpdateCanvas cid -> do invalidateInBBox Nothing Efficient cid
                             minibufStart drawwdw (srcsfc,tgtsfc) strks
      OkCancel True -> (return . Right) (toList strks)
      OkCancel False -> (return . Right) []
      ChangeDialog -> return (Left ())
      MiniBuffer (MiniBufferPenDown PenButton1 pcoord) -> do 
        ps <- onestroke drawwdw (srcsfc,tgtsfc) (singleton pcoord) 
        let nstrks = strks |> mkstroke ps
        liftIO $ renderWith srcsfc (drawMiniBuf nstrks)
        minibufStart drawwdw (srcsfc,tgtsfc) nstrks
      _ -> minibufStart drawwdw (srcsfc,tgtsfc) strks
      
onestroke :: DrawWindow -> (Surface,Surface) -> Seq PointerCoord 
          -> MainCoroutine (Seq PointerCoord)
onestroke drawwdw (srcsfc,tgtsfc) pcoords = do 
    r <- nextevent 
    case r of 
      MiniBuffer (MiniBufferPenMove pcoord) -> do 
        let newpcoords = pcoords |> pcoord 
        liftIO $ do drawstrokebit (srcsfc,tgtsfc) newpcoords
                    invalidateMinibuf drawwdw tgtsfc
        onestroke drawwdw (srcsfc,tgtsfc) newpcoords
      MiniBuffer (MiniBufferPenUp pcoord) -> return (pcoords |> pcoord)
      _ -> onestroke drawwdw (srcsfc,tgtsfc) pcoords

drawstrokebit :: (Surface,Surface) -> Seq PointerCoord -> IO()
drawstrokebit (srcsfc,tgtsfc) ps = 
    renderWith tgtsfc $ do 
      setSourceSurface srcsfc 0 0
      setOperator OperatorSource 
      paint 
      case viewl ps of
        p :< ps' -> do 
          setOperator OperatorOver 
          setSourceRGBA 0.0 0.0 0.0 1.0
          setLineWidth (view penWidth defaultPenWCS) 
          moveTo (pointerX p) (pointerY p)
          mapM_ (uncurry lineTo . ((,)<$>pointerX<*>pointerY)) ps'
          stroke 
        _ -> return ()
 
mkstroke :: Seq PointerCoord -> Stroke
mkstroke ps = let xyzs = fmap ((,,) <$> pointerX <*> pointerY <*> const 1.0) ps
                  pinfo = defaultPenInfo
              in createNewStroke pinfo xyzs
                  

