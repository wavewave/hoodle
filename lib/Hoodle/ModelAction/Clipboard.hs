-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.ModelAction.Clipboard 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Clipboard io actions
-- 
-----------------------------------------------------------------------------

module Hoodle.ModelAction.Clipboard where

import Control.Lens 
import Control.Monad.Trans
import qualified Data.ByteString.Base64 as B64 
import qualified Data.ByteString.Char8 as C8
import qualified Data.Serialize as Se 
import Graphics.UI.Gtk hiding (get,set)
--
import           Data.Xournal.BBox
-- 
import           Hoodle.ModelAction.Select
import           Hoodle.Script.Hook
import           Hoodle.Type.Event 
import           Hoodle.Type.XournalState 
--

-- | 
updateClipboard :: HoodleState -> [StrokeBBox] -> IO HoodleState 
updateClipboard xstate strs 
  | null strs = return xstate
  | otherwise = do 
    let ui = view gtkUIManager xstate
    hdltag <- atomNew "hoodle"
    -- tgttag <- atomNew "Stroke"
    -- seltag <- atomNew "Stroke"
    clipbd <- clipboardGet hdltag
    let bstr = C8.unpack . B64.encode . Se.encode $ strs 
    clipboardSetText clipbd bstr
    togglePaste ui True 
    case (view hookSet xstate) of 
      Nothing -> return () 
      Just hset -> case afterUpdateClipboardHook hset of 
                     Nothing -> return () 
                     Just uchook -> liftIO $ uchook strs 
    return xstate



-- |
callback4Clip :: (MyEvent -> IO ()) -> Maybe String -> IO ()
callback4Clip callbk Nothing = callbk (GotClipboardContent Nothing)
callback4Clip callbk (Just str) = do
    let r = do let bstr = C8.pack str 
               bstr' <- B64.decode bstr
               Se.decode bstr' 
    case r of 
      Left err -> callbk (GotClipboardContent Nothing)
      Right cnt -> callbk (GotClipboardContent (Just cnt))
