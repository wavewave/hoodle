{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Minibuffer 
-- Copyright   : (c) 2013 Ian-Woo Kim
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
import           Data.Sequence (Seq,(|>),empty,singleton,viewr,ViewR(..))
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
import           Hoodle.Type.Canvas (defaultPenInfo)
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
    action dev doesUseX11Ext = mkIOaction $ \evhandler -> do 
      dialog <- dialogNew 
      msgLabel <- labelNew (Just msg) 
      cvs <- drawingAreaNew                           
      cvs `on` sizeRequest $ return (Requisition 500 50)
      cvs `on` exposeEvent $ tryEvent $ do
        drawwdw <- liftIO $ widgetGetDrawWindow cvs                 
        liftIO (renderWithDrawable drawwdw drawMiniBufBkg)
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
      vbox <- dialogGetUpper dialog
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
minibufInit = do 
--  r <- nextevent
                   
  waitSomeEvent (\case MiniBuffer (MiniBufferInitialized _ )-> True ; _ -> False) 
  >>= (\case MiniBuffer (MiniBufferInitialized drawwdw) -> 
               minibufStart drawwdw empty 
             _ -> minibufInit)
{-  case r of  
    MiniBuffer (MiniBufferInitialized drawwdw) -> minibufStart drawwdw empty 
    _ -> minibufInit -}

invalidateMinibuf :: DrawWindow -> Seq Stroke -> MainCoroutine ()
invalidateMinibuf drawwdw strks = liftIO $ renderWithDrawable drawwdw (drawMiniBuf strks)

minibufStart :: DrawWindow -> Seq Stroke -> MainCoroutine (Either () [Stroke])
minibufStart drawwdw strks = do 
    invalidateMinibuf drawwdw strks
    r <- nextevent 
    case r of 
      UpdateCanvas cid -> invalidateInBBox Nothing Efficient cid >> minibufStart drawwdw strks
      OkCancel True -> (return . Right) (toList strks)
      OkCancel False -> (return . Right) []
      ChangeDialog -> return (Left ())
      MiniBuffer (MiniBufferPenDown PenButton1 pcoord) -> do 
        ps <- onestroke drawwdw (singleton pcoord) 
        minibufStart drawwdw (strks |> mkstroke ps) 
      _ -> minibufStart drawwdw strks
      
onestroke :: DrawWindow -> Seq PointerCoord -> MainCoroutine (Seq PointerCoord)
onestroke drawwdw pcoords = do 
    r <- nextevent 
    case r of 
      MiniBuffer (MiniBufferPenMove pcoord) -> do 
        let newpcoords = pcoords |> pcoord 
        drawstrokebit drawwdw newpcoords
        onestroke drawwdw newpcoords
      MiniBuffer (MiniBufferPenUp pcoord) -> return (pcoords |> pcoord)
      _ -> onestroke drawwdw pcoords

drawstrokebit :: DrawWindow -> Seq PointerCoord -> MainCoroutine ()
drawstrokebit drawwdw ps = 
    case viewr ps of
      ps' :> c0 -> case viewr ps' of 
        _ps'' :> c1 -> liftIO . renderWithDrawable drawwdw $ do 
                         setLineWidth 1.0
                         moveTo (pointerX c1) (pointerY c1)
                         lineTo (pointerX c0) (pointerY c0)
                         stroke
        _ -> return ()
      _ -> return ()
 
mkstroke :: Seq PointerCoord -> Stroke
mkstroke ps = let xyzs = fmap ((,,) <$> pointerX <*> pointerY <*> const 1.0) ps
                  pinfo = defaultPenInfo
              in createNewStroke pinfo xyzs
                  

{-  
    waitSomeEvent (\case OkCancel b -> True 
                         ChangeDialog -> True
                         _ -> False)
    >>= \case OkCancel b -> (return . Right) []  
              ChangeDialog -> return (Left ())
              _ -> return (Right [])  
-}