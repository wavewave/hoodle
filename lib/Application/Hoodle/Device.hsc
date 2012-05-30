{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Application.Hoodle.Device 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

#include <gtk/gtk.h>
#include "template-hsc-gtk2hs.h"

module Application.Hoodle.Device where

import Application.Hoodle.Config
import Data.Configurator.Types
import Control.Applicative 
import Control.Monad.Reader
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import Graphics.UI.Gtk
import Data.Int

-- | 

data PointerType = Core | Stylus | Eraser
                 deriving (Show,Eq,Ord)

-- |

data PenButton = PenButton1 | PenButton2 | PenButton3 | EraserButton
               deriving (Show,Eq,Ord)

-- | 

data DeviceList = DeviceList { dev_core :: CInt
                             , dev_stylus :: CInt
                             , dev_eraser :: CInt } 
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
  :: Ptr CInt -> Ptr CInt -> Ptr CInt -> CString -> CString -> CString -> IO ()

-- | 

initDevice :: Config -> IO DeviceList  
initDevice cfg = do 
  (mcore,mstylus,meraser) <- getPenDevConfig cfg 
  putStrLn $ show mstylus 
  putStrLn $ show meraser
  with 0 $ \pcore -> 
    with 0 $ \pstylus -> 
      with 0 $ \peraser -> do 
        pcorename <- case mcore of 
                       Nothing -> newCString "Core Pointer"
                       Just core -> newCString core
        pstylusname <- case mstylus of 
                         Nothing -> newCString "stylus"
                         Just spen -> newCString spen
        perasername <- case meraser of 
                         Nothing -> newCString "eraser"
                         Just seraser -> newCString seraser 
                         
        c_initdevice pcore pstylus peraser pcorename pstylusname perasername
        
        core_val <- peek pcore
        stylus_val <- peek pstylus
        eraser_val <- peek peraser
        return $ DeviceList core_val stylus_val eraser_val
                 
-- |

getPointer :: DeviceList -> EventM t (Maybe PenButton,PointerCoord)
getPointer devlst = do 
    ptr <- ask 
    (_ty,btn,x,y,mdev,maxf) <- liftIO (getInfo ptr)
    let rbtn | btn == 0 = Nothing 
             | btn == 1 = Just PenButton1
             | btn == 2 = Just PenButton2 
             | btn == 3 = Just PenButton3
             | otherwise = Nothing 
    case mdev of 
      Nothing -> return (rbtn,PointerCoord Core x y 1.0)
      Just dev -> case maxf of 
                    Nothing -> return (rbtn,PointerCoord Core x y 1.0)
                    Just axf -> do 
                      pcoord <- liftIO $ coord ptr x y dev axf 
                      let rbtnfinal = case pointerType pcoord of 
                                        Eraser -> Just EraserButton
                                        _ -> rbtn 
                      
                      let tst = (rbtnfinal,pcoord)
                        -- (,) rbtn <$> (liftIO $ coord ptr x y dev axf)
                      -- liftIO $ print tst 
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
          | device == dev_core devlst = return $ PointerCoord Core x y 1.0 
          | device == dev_stylus devlst = do 
            (ptrax :: Ptr CDouble ) <- axf ptr 
            (wacomx :: Double) <- peekByteOff ptrax 0
            (wacomy :: Double) <- peekByteOff ptrax 8
            (wacomz :: Double) <- peekByteOff ptrax 16
            return $ PointerCoord Stylus wacomx wacomy wacomz
          | device == dev_eraser devlst = do 
            (ptrax :: Ptr CDouble ) <- axf ptr 
            (wacomx :: Double) <- peekByteOff ptrax 0
            (wacomy :: Double) <- peekByteOff ptrax 8
            (wacomz :: Double) <- peekByteOff ptrax 16 
            return $ PointerCoord Eraser wacomx wacomy wacomz 
          | otherwise = return $ PointerCoord Core x y 1.0

-- | 
    
wacomCoordConvert :: WidgetClass self => self 
                     -> (Double,Double) 
                     -> IO (Double,Double)
wacomCoordConvert canvas (x,y)= do 
  win <- widgetGetDrawWindow canvas
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

