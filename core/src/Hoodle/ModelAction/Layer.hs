{-# LANGUAGE CPP #-}

-- |
-- Module      : Hoodle.ModelAction.Layer
-- Copyright   : (c) 2011-2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
module Hoodle.ModelAction.Layer where

import Control.Category ((.))
import Control.Lens (over, view)
import Data.Hoodle.Generic (glayers)
import Data.Hoodle.Zipper (current, replace)
import Data.IORef (IORef, modifyIORef)
import Graphics.Hoodle.Render.Type (RLayer)
import qualified Graphics.UI.Gtk as Gtk
import Hoodle.Type.Alias (EditMode, Page)
import Hoodle.Util (maybeRead)
import Prelude hiding (id, (.))

-- |
getCurrentLayer :: Page EditMode -> RLayer
getCurrentLayer = current . view glayers

-- |
adjustCurrentLayer :: RLayer -> Page EditMode -> Page EditMode
adjustCurrentLayer nlayer = over glayers (replace nlayer)

-- |
layerChooseDialog :: IORef Int -> Int -> Int -> IO Gtk.Dialog
layerChooseDialog layernumref cidx len = do
  dialog <- Gtk.dialogNew
  layerentry <- Gtk.entryNew
  Gtk.entrySetText layerentry (show (succ cidx))
  label <- Gtk.labelNew (Just (" / " ++ show len))
  hbox <- Gtk.hBoxNew False 0
  upper <- fmap Gtk.castToContainer (Gtk.dialogGetContentArea dialog)
  Gtk.containerAdd upper hbox
  Gtk.boxPackStart hbox layerentry Gtk.PackNatural 0
  Gtk.boxPackStart hbox label Gtk.PackGrow 0
  Gtk.widgetShowAll upper
  buttonOk <- Gtk.dialogAddButton dialog Gtk.stockOk Gtk.ResponseOk
  _buttonCancel <- Gtk.dialogAddButton dialog Gtk.stockCancel Gtk.ResponseCancel

  _ <- buttonOk `Gtk.on` Gtk.buttonActivated $ do
    txt <- Gtk.get layerentry Gtk.entryText
    maybe (return ()) (modifyIORef layernumref . const . pred) . maybeRead $ txt
  return dialog
