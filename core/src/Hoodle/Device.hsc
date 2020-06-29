{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include <gtk/gtk.h>
#include "template-hsc-gtk2hs.h"

module Hoodle.Device where

import Control.Applicative 
import Control.Monad.Reader
import Data.Configurator.Types
import Data.Int
import Foreign.C
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.UI.Gtk
--
import Hoodle.Config

-- | 
data PointerType = Core | Stylus | Eraser | Touch 
                 deriving (Show,Eq,Ord)

-- |
data PenButton = PenButton1 | PenButton2 | PenButton3 | EraserButton | TouchButton
               deriving (Show,Eq,Ord)

-- | 

data DeviceList = DeviceList { dev_core       :: CInt 
                             , dev_core_str   :: String
                             , dev_stylus     :: CInt
                             , dev_stylus_str :: String
                             , dev_eraser     :: CInt 
                             , dev_eraser_str :: String
                             , dev_touch      :: CInt  
                             , dev_touch_str  :: String
                             } 
                deriving Show 
                  
-- | 

data PointerCoord = PointerCoord { pointerType :: PointerType 
                                 , pointerX :: Double 
                                 , pointerY :: Double 
                                 , pointerZ :: Double
                                 } 
                  | NoPointerCoord
                  deriving (Show,Eq,Ord)

-- | 
foreign import ccall "c_initdevice.h initdevice" c_initdevice
  :: Ptr CInt -- ^ core 
  -> Ptr CInt -- ^ stylus
  -> Ptr CInt -- ^ eraser
  -> Ptr CInt -- ^ touch 
  -> CString  -- ^ core 
  -> CString  -- ^ stylus
  -> CString  -- ^ eraser
  -> CString  -- ^ touch 
  -> IO ()

-- | 
initDevice :: Config -> IO DeviceList  
initDevice cfg = do 
  pstylusname_detect <- newCString "stylus" 
  perasername_detect <- newCString "eraser" 
  ptouchname_detect <- newCString "touch"
  (mcore,mstylus,meraser,mtouch) <- getPenDevConfig cfg 
  with 0 $ \pcore -> 
    with 0 $ \pstylus -> 
      with 0 $ \peraser -> do 
        with 0 $ \ptouch -> do 
          (pcorename,corename) <- case mcore of 
            Nothing -> (,) <$> newCString "Virtual core pointer" <*> pure "Virtual core pointer"
            Just core -> (,) <$> newCString core <*> pure core
          (pstylusname,stylusname) <- case mstylus of 
            Nothing -> return (pstylusname_detect,"stylus")
            Just spen -> (,) <$> newCString spen <*> pure spen 
          (perasername,erasername) <- case meraser of 
            Nothing -> return (perasername_detect,"eraser")
            Just seraser -> (,) <$> newCString seraser <*> pure seraser
          (ptouchname,touchname) <- 
            maybe (return (ptouchname_detect,"touch")) (\stouch->(,) <$> newCString stouch <*> pure stouch) 
                  mtouch 
                            

          c_initdevice pcore pstylus peraser ptouch pcorename pstylusname perasername ptouchname

          core_val <- peek pcore
          stylus_val <- peek pstylus
          eraser_val <- peek peraser
          touch_val <- peek ptouch
          
          return $ DeviceList core_val corename stylus_val stylusname eraser_val erasername touch_val touchname  
                 
-- |
getPointer :: DeviceList -> EventM t (Maybe PenButton,Maybe PointerCoord)
getPointer devlst = do 
    ptr <- ask 
    (_ty,btn,x,y,mdev,maxf) <- liftIO (getInfo ptr)
    let rbtn | btn == 0 = Nothing 
             | btn == 1 = Just PenButton1
             | btn == 2 = Just PenButton2 
             | btn == 3 = Just PenButton3
             | otherwise = Nothing 
    case mdev of 
      Nothing  -> return (rbtn,Nothing)
      Just dev -> case maxf of 
                    Nothing -> return (rbtn,Just (PointerCoord Core x y 1.0))
                    Just axf -> do 
                      mpcoord <- liftIO $ coord ptr x y dev axf
                      let rbtnfinal = case mpcoord of 
                                        Nothing -> rbtn 
                                        Just pcoord -> case pointerType pcoord of 
                                                         Eraser -> Just EraserButton
                                                         Touch  -> Just TouchButton
                                                         _ -> rbtn 
                      
                      let tst = (rbtnfinal,mpcoord)
                      return tst 
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
          (btn :: #{gtk2hs_type gint}) <- #{peek GdkEventButton, button} ptr
          (dev :: CInt) <- #{peek GdkEventButton, device} ptr
          let axisfunc = #{peek GdkEventButton, axes}
          return (ty,btn,realToFrac x,realToFrac y,Just dev,Just axisfunc)
        else if ty `elem` [ #{const GDK_SCROLL} ] 
        then do
          (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventScroll, x} ptr
          (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventScroll, y} ptr
          (dev :: CInt) <- #{peek GdkEventScroll, device} ptr
          return (ty,0,realToFrac x, realToFrac y,Just dev,Nothing)
        else if ty `elem` [ #{const GDK_MOTION_NOTIFY} ] 
        then do
          (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventMotion, x} ptr
          (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventMotion, y} ptr
          (dev :: CInt) <- #{peek GdkEventMotion, device} ptr
          let axisfunc = #{peek GdkEventMotion, axes}          
          return (ty,0,realToFrac x, realToFrac y,Just dev,Just axisfunc)
        else if ty `elem` [ #{const GDK_ENTER_NOTIFY},
                            #{const GDK_LEAVE_NOTIFY}] 
        then do
          (x :: #{gtk2hs_type gdouble}) <- #{peek GdkEventCrossing, x} ptr
          (y :: #{gtk2hs_type gdouble}) <- #{peek GdkEventCrossing, y} ptr
          return (ty,0,realToFrac x, realToFrac y,Nothing,Nothing)
        else error ("eventCoordinates: none for event type "++show ty)

    coord ptr x y device axf 
          | device == dev_core devlst = return $ Just (PointerCoord Core x y 1.0)
          | device == dev_stylus devlst = do 
            (ptrax :: Ptr CDouble ) <- axf ptr 
            (wacomx :: Double) <- peekByteOff ptrax 0
            (wacomy :: Double) <- peekByteOff ptrax 8
            (wacomz :: Double) <- peekByteOff ptrax 16
            return $ Just (PointerCoord Stylus wacomx wacomy wacomz)
          | device == dev_eraser devlst = do 
            (ptrax :: Ptr CDouble ) <- axf ptr 
            (wacomx :: Double) <- peekByteOff ptrax 0
            (wacomy :: Double) <- peekByteOff ptrax 8
            (wacomz :: Double) <- peekByteOff ptrax 16 
            return $ Just (PointerCoord Eraser wacomx wacomy wacomz)
          | device == dev_touch devlst = do 
            (ptrax :: Ptr CDouble ) <- axf ptr 
            (touchx :: Double) <- peekByteOff ptrax 0
            (touchy :: Double) <- peekByteOff ptrax 8
            (touchz :: Double) <- peekByteOff ptrax 16 
            -- (touchw :: Double) <- peekByteOff ptrax 24
            return $ Just (PointerCoord Touch touchx touchy touchz)            
          | otherwise = return Nothing -- return $ PointerCoord Core x y 1.0

-- | 
    
wacomCoordConvert :: WidgetClass self => self 
                     -> (Double,Double) 
                     -> IO (Double,Double)
wacomCoordConvert canvas (x,y)= do 
  -- #ifdef GTK3  
  Just win <- widgetGetWindow canvas -- partial function for the time being 
  -- #else // GTK3
  -- win <- widgetGetDrawWindow canvas
  -- #endif // GTK3
  (x0,y0) <- drawWindowGetOrigin win
  screen <- widgetGetScreen canvas
  (ws,hs) <- (,) <$> screenGetWidth screen <*> screenGetHeight screen
  return (fromIntegral ws*x-fromIntegral x0,fromIntegral hs*y-fromIntegral y0)
  
-- | 
  
wacomPConvert ::  WidgetClass self => self 
                  -> PointerCoord 
                  -> IO (Double,Double)
wacomPConvert canvas pcoord = do 
 let (px,py) = (,) <$> pointerX <*> pointerY $ pcoord  
 case pointerType pcoord of 
   Core -> return (px,py)
   _ -> do 
     wacomCoordConvert canvas (px,py)

