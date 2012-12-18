{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.ContextMenu
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.ContextMenu where

-- from other packages
import           Control.Category
import           Control.Lens
import           Control.Monad.State
import           Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy as L
import qualified Data.IntMap as IM
import           Graphics.Rendering.Cairo
import           Graphics.UI.Gtk hiding (get,set)
import           System.Directory
import           System.FilePath
-- from hoodle-platform
import           Control.Monad.Trans.Crtn.Event
import           Control.Monad.Trans.Crtn.Queue 
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple
import           Data.Hoodle.Select
-- import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Generic
import           Graphics.Hoodle.Render.Item
import           Graphics.Hoodle.Render.Type
import           Graphics.Hoodle.Render.Type.HitTest 
import           Text.Hoodle.Builder 
-- from this package 
import           Hoodle.Accessor
import           Hoodle.Coroutine.File
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
--
import Prelude hiding ((.),id)

processContextMenu :: String -> MainCoroutine () 
processContextMenu str = okMessageBox str 

showContextMenu :: MainCoroutine () 
showContextMenu = modify (tempQueue %~ enqueue action) 
                  >> waitSomeEvent (==ContextMenuCreated) 
                  >> return () 
  where action = Left . ActionOrder $ 
                   \evhandler -> do 
                     menu <- menuNew 
                     menuSetTitle menu "MyMenu"
                     menuitem1 <- menuItemNewWithLabel "test1"
                     menuitem2 <- menuItemNewWithLabel "test2"
                     menuitem1 `on` menuItemActivate $ do  
                       -- liftIO $ putStrLn "test1 called" 
                       evhandler (GotContextMenuSignal "test1 called")
                       return ()
                     menuitem2 `on` menuItemActivate $ do   
                       -- liftIO $ putStrLn "test2 called" 
                       evhandler (GotContextMenuSignal "test2 called") 
                       return ()
                     menuAttach menu menuitem1 0 1 0 1 
                     menuAttach menu menuitem2 0 1 1 2
                     widgetShowAll menu 
                     menuPopup menu Nothing 
                     putStrLn "showContextMenu"
                     return ContextMenuCreated 

