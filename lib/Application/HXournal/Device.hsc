{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

#include <gtk/gtk.h>
#include "template-hsc-gtk2hs.h"

module Application.HXournal.Device where

import Control.Applicative 
import Control.Monad.Reader

import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.C
import Foreign.Storable

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM

import Data.Int

data PointerType = Core | Stylus | Eraser
                 deriving (Show,Eq,Ord)

data DeviceList = DeviceList { dev_core :: CInt
                             , dev_stylus :: CInt
                             , dev_eraser :: CInt } 
                deriving Show 
                  
data PointerCoord = PointerCoord { pointerType :: PointerType 
                                 , pointerX :: Double 
                                 , pointerY :: Double } 
                  | NoPointerCoord
                  deriving (Show,Eq,Ord)


foreign import ccall "c_initdevice.h initdevice" c_initdevice
  :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

initDevice :: IO DeviceList  
initDevice = 
  with 0 $ \pcore -> 
  with 0 $ \pstylus -> 
  with 0 $ \peraser -> do 
        c_initdevice pcore pstylus peraser
        core_val <- peek pcore
        stylus_val <- peek pstylus
        eraser_val <- peek peraser
        return $ DeviceList core_val stylus_val eraser_val
                 

getPointer :: DeviceList -> EventM t PointerCoord
getPointer devlst = do 
    ptr <- ask 
    (ty,x,y,mdev,maxf) <- liftIO (getInfo ptr)
    -- liftIO $ print (ty,x,y,mdev)
    case mdev of 
      Nothing -> return (PointerCoord Core x y)
      Just dev -> case maxf of 
                    Nothing -> return (PointerCoord Core x y)
                    Just axf -> liftIO $ coord ptr x y dev axf     
  where 
    getInfo ptr = do 
      (ty :: #{gtk2hs_type GdkEventType}) <- peek (castPtr ptr)
      if ty `elem` [ #{const GDK_BUTTON_PRESS}
                   , #{const GDK_2BUTTON_PRESS}
                   , #{const GDK_3BUTTON_PRESS}
                   , #{const GDK_BUTTON_RELEASE}] 
        then do 
          (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventButton, x} ptr 
          (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventButton, y} ptr
          (dev :: CInt) <- #{peek GdkEventButton, device} ptr
          let axisfunc = #{peek GdkEventButton, axes}
          return (ty,realToFrac x,realToFrac y,Just dev,Just axisfunc)
        else if ty `elem` [ #{const GDK_SCROLL} ] 
        then do
          (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventScroll, x} ptr
          (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventScroll, y} ptr
          (dev :: CInt) <- #{peek GdkEventScroll, device} ptr
          return (ty,realToFrac x, realToFrac y,Just dev,Nothing)
        else if ty `elem` [ #{const GDK_MOTION_NOTIFY} ] 
        then do
          (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventMotion, x} ptr
          (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventMotion, y} ptr
          (dev :: CInt) <- #{peek GdkEventMotion, device} ptr
          let axisfunc = #{peek GdkEventMotion, axes}          
          return (ty,realToFrac x, realToFrac y,Just dev,Just axisfunc)
        else if ty `elem` [ #{const GDK_ENTER_NOTIFY},
                            #{const GDK_LEAVE_NOTIFY}] 
        then do
          (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventCrossing, x} ptr
          (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventCrossing, y} ptr
          return (ty, realToFrac x, realToFrac y,Nothing,Nothing)
        else error ("eventCoordinates: none for event type "++show ty)

    coord ptr x y device axf 
          | device == dev_core devlst = return $ PointerCoord Core x y 
          | device == dev_stylus devlst = do 
            (ptrax :: Ptr CDouble ) <- axf ptr 
            (wacomx :: Double) <- peekByteOff ptrax 0
            (wacomy :: Double) <- peekByteOff ptrax 8
            return $ PointerCoord Stylus wacomx wacomy 
          | device == dev_eraser devlst = do 
            (ptrax :: Ptr CDouble ) <- axf ptr 
            (wacomx :: Double) <- peekByteOff ptrax 0
            (wacomy :: Double) <- peekByteOff ptrax 8
            return $ PointerCoord Eraser wacomx wacomy 
          | otherwise = return $ PointerCoord Core x y

wacomCoordConvert :: WidgetClass self => self 
                     -> (Double,Double) 
                     -> IO (Double,Double)
wacomCoordConvert canvas (x,y)= do 
  win <- widgetGetDrawWindow canvas
  (x0,y0) <- drawWindowGetOrigin win
  screen <- widgetGetScreen canvas
  (ws,hs) <- (,) <$> screenGetWidth screen <*> screenGetHeight screen
  return (fromIntegral ws*x-fromIntegral x0,fromIntegral hs*y-fromIntegral y0)
  
wacomPConvert ::  WidgetClass self => self 
                  -> PointerCoord 
                  -> IO (Double,Double)
wacomPConvert canvas pcoord = do 
 let (px,py) = (,) <$> pointerX <*> pointerY $ pcoord  
 case pointerType pcoord of 
   Core -> return (px,py)
   _ -> do 
     wacomCoordConvert canvas (px,py)

{-
data Csr = Csr
type CsrPtr = Ptr Csr

foreign import ccall "gdk_window_set_cursor" gdkWindowSetCursor 
  :: Ptr DrawWindow -> CsrPtr -> IO ()

foreign import ccall "gdk_cursor_new" gdkCursorNew 
  :: Int -> IO CsrPtr

setCursor :: DrawWindow -> IO ()
setCursor window = do
  c <- gdkCursorNew 34 -- the 'crosshair' cursor
  d <- widgetGetDrawWindow window
  withForeignPtr (toDrawWindow d) $ \ptr -> gdkWindowSetCursor ptr c
-}