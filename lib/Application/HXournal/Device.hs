{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

module Application.HXournal.Device where

import Control.Applicative 
import Control.Monad.Reader

import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.C
import Foreign.Storable

import Graphics.UI.Gtk

data PointerType = Core | Stylus | Eraser
                 deriving (Show,Eq,Ord)

data DeviceList = DeviceList { dev_core :: CInt
                             , dev_stylus :: CInt
                             , dev_eraser :: CInt } 
                deriving Show 
                  
data PointerCoord = PointerCoord { pointerType :: PointerType 
                                 , pointerX :: Double 
                                 , pointerY :: Double } 
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
                 

getPointer :: DeviceList 
              -> EventM t PointerCoord
getPointer dev = do 
    ptr <- ask 
    liftIO $ do 
      (x :: Double) <- peekByteOff ptr 16
      (y :: Double) <- peekByteOff ptr 24
      (device :: CInt ) <- peekByteOff ptr 44
      coord ptr x y device 
  where coord ptr x y device 
          | device == dev_core dev = return $ PointerCoord Core x y 
          | device == dev_stylus dev = do 
            (ptrax :: Ptr CDouble ) <- peekByteOff ptr 32
            (wacomx :: Double) <- peekByteOff ptrax 0
            (wacomy :: Double) <- peekByteOff ptrax 8
            return $ PointerCoord Stylus wacomx wacomy 
          | device == dev_eraser dev = do 
            (ptrax :: Ptr CDouble ) <- peekByteOff ptr 32
            (wacomx :: Double) <- peekByteOff ptrax 0
            (wacomy :: Double) <- peekByteOff ptrax 8
            return $ PointerCoord Eraser wacomx wacomy 
          | otherwise = error "no such device"

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