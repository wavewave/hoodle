-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.ModelAction.ContextMenu
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.ModelAction.ContextMenu where

import qualified Data.ByteString.Char8 as B
import Data.UUID.V4
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import System.Directory 
import System.FilePath 
-- 
-- import Control.Monad.Trans.Crtn.Driver 
import Data.Hoodle.BBox
import Data.Hoodle.Simple
import Graphics.Hoodle.Render 
import Graphics.Hoodle.Render.Type.Item
--
import Hoodle.Type.Event 

-- | 
menuCreateALink :: (MyEvent -> IO ()) -> [RItem] -> IO (Maybe MenuItem)
menuCreateALink evhandler sitems = 
  if (length . filter isLinkInRItem) sitems > 0
  then return Nothing 
  else do mi <- menuItemNewWithLabel "Create a link to..." 
          mi `on` menuItemActivate $ 
            evhandler (GotContextMenuSignal CMenuCreateALink)
          return (Just mi)
         
-- |
makeSVGFromSelection :: [RItem] -> BBox -> IO SVG 
makeSVGFromSelection hititms (BBox (ulx,uly) (lrx,lry)) = do 
  uuid <- nextRandom
  tdir <- getTemporaryDirectory
  let filename = tdir </> show uuid <.> "svg"
      (x,y) = (ulx,uly)
      (w,h) = (lrx-ulx,lry-uly)
  withSVGSurface filename w h $ \s -> renderWith s $ do 
    translate (-ulx) (-uly) 
    mapM_ renderRItem hititms 
  bstr <- B.readFile filename
  let svg = SVG Nothing Nothing bstr (x,y) (Dim w h)
  svg `seq` removeFile filename 
  return svg                       

