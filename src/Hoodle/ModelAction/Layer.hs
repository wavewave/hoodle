-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.ModelAction.Layer 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.ModelAction.Layer where

-- from other packages
import           Control.Category
import           Control.Lens (view,over)
import           Data.IORef
import           Graphics.UI.Gtk hiding (get,set)
import qualified Graphics.UI.Gtk as Gtk (get)
-- from hoodle-platform 
import           Data.Hoodle.Generic
import           Data.Hoodle.Zipper
import           Graphics.Hoodle.Render.Type
-- 
import           Hoodle.Util
import           Hoodle.Type.Alias
-- 
import Prelude hiding ((.),id)


-- |
getCurrentLayer :: Page EditMode -> RLayer
getCurrentLayer = current . view glayers 



-- | 
adjustCurrentLayer :: RLayer -> Page EditMode -> Page EditMode
adjustCurrentLayer nlayer = over glayers (replace nlayer)


-- | 
layerChooseDialog :: IORef Int -> Int -> Int -> IO Dialog
layerChooseDialog layernumref cidx len = do 
    dialog <- dialogNew 
    layerentry <- entryNew
    entrySetText layerentry (show (succ cidx))
    label <- labelNew (Just (" / " ++ show len))
    hbox <- hBoxNew False 0 
    upper <- dialogGetUpper dialog
    boxPackStart upper hbox PackNatural 0 
    boxPackStart hbox layerentry PackNatural 0 
    boxPackStart hbox label PackGrow 0 
    widgetShowAll upper
    buttonOk <- dialogAddButton dialog stockOk ResponseOk
    _buttonCancel <- dialogAddButton dialog stockCancel ResponseCancel

    buttonOk `on` buttonActivated $ do 
      txt <- Gtk.get layerentry entryText
      maybe (return ()) (modifyIORef layernumref . const . pred) . maybeRead $ txt
    return dialog


