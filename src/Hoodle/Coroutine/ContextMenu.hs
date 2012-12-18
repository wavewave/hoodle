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
import           Data.Monoid
import           Graphics.Rendering.Cairo
import           Graphics.UI.Gtk hiding (get,set)
import           System.Directory
import           System.FilePath
-- from hoodle-platform
import           Control.Monad.Trans.Crtn.Event
import           Control.Monad.Trans.Crtn.Queue 
import           Data.Hoodle.BBox
import           Data.Hoodle.Generic
import           Data.Hoodle.Simple
import           Data.Hoodle.Select
import           Graphics.Hoodle.Render
-- import           Graphics.Hoodle.Render.Generic
-- import           Graphics.Hoodle.Render.Item
import           Graphics.Hoodle.Render.Type
import           Graphics.Hoodle.Render.Type.HitTest 
-- import           Text.Hoodle.Builder 
-- from this package 
import           Hoodle.Accessor
import           Hoodle.Coroutine.File
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState
--
import Prelude hiding ((.),id)

processContextMenu :: ContextMenuEvent -> MainCoroutine () 
processContextMenu ctxtmenu = do 
  xstate <- get
  case view hoodleModeState xstate of 
    SelectState thdl -> do 
      liftIO $ putStrLn "SelectState"
      case view gselSelected thdl of 
        Nothing -> return () 
        Just (_,tpg) -> do 
          let ealist = unTEitherAlterHitted 
                       . view gitems . view selectedLayer . view glayers $ tpg 
          case ealist of 
            Right alist -> do 
              let hititms = concatMap unHitted (getB alist) 
                  ulbbox = unUnion . mconcat . fmap (Union . Middle . getBBox) 
                           $ hititms 
              case ulbbox of 
                Middle bbox -> 
                  case ctxtmenu of 
                    ContextMenuMakeSVG -> exportCurrentSelectionAsSVG hititms bbox
                    ContextMenuMakePDF -> exportCurrentSelectionAsPDF hititms bbox
                _ -> return () 
            Left _ -> do liftIO $ putStrLn "not exist"
    _ -> return () 

exportCurrentSelectionAsSVG :: [RItem] -> BBox -> MainCoroutine () 
exportCurrentSelectionAsSVG hititms bbox@(BBox (ulx,uly) (lrx,lry)) = 
    fileChooser FileChooserActionSave >>= maybe (return ()) action 
  where 
    action filename =
      -- this is rather temporary not to make mistake 
      if takeExtension filename /= ".svg" 
      then fileExtensionInvalid (".svg","export") 
           >> exportCurrentSelectionAsSVG hititms bbox
      else do      
        liftIO $ print "exportCurrentSelectionAsSVG executed"
        liftIO $ withSVGSurface filename (lrx-ulx) (lry-uly) $ \s -> renderWith s $ do 
          translate (-ulx) (-uly)
          mapM_ renderRItem  hititms


exportCurrentSelectionAsPDF :: [RItem] -> BBox -> MainCoroutine () 
exportCurrentSelectionAsPDF hititms bbox@(BBox (ulx,uly) (lrx,lry)) = 
    fileChooser FileChooserActionSave >>= maybe (return ()) action 
  where 
    action filename =
      -- this is rather temporary not to make mistake 
      if takeExtension filename /= ".pdf" 
      then fileExtensionInvalid (".svg","export") 
           >> exportCurrentSelectionAsPDF hititms bbox
      else do      
        liftIO $ print "exportCurrentSelectionAsPDF executed"
        liftIO $ withPDFSurface filename (lrx-ulx) (lry-uly) $ \s -> renderWith s $ do 
          translate (-ulx) (-uly)
          mapM_ renderRItem  hititms


showContextMenu :: MainCoroutine () 
showContextMenu = modify (tempQueue %~ enqueue action) 
                  >> waitSomeEvent (==ContextMenuCreated) 
                  >> return () 
  where action = Left . ActionOrder $ 
                   \evhandler -> do 
                     menu <- menuNew 
                     menuSetTitle menu "MyMenu"
                     menuitem1 <- menuItemNewWithLabel "Make SVG"
                     menuitem2 <- menuItemNewWithLabel "Make PDF"
                     menuitem1 `on` menuItemActivate $ do  
                       evhandler (GotContextMenuSignal ContextMenuMakeSVG)
                       return ()
                     menuitem2 `on` menuItemActivate $ do   
                       evhandler (GotContextMenuSignal ContextMenuMakePDF) 
                       return () 
                     menuAttach menu menuitem1 0 1 0 1 
                     menuAttach menu menuitem2 0 1 1 2
                     widgetShowAll menu 
                     menuPopup menu Nothing 
                     putStrLn "showContextMenu"
                     return ContextMenuCreated 

