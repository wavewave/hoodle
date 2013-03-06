-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.ModelAction.Clipboard 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
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

-- from other package
import           Control.Lens (view,set,over)
import           Control.Monad.Trans
import qualified Data.ByteString.Base64 as B64 
import qualified Data.ByteString.Char8 as C8
import qualified Data.Serialize as Se 
import           Graphics.UI.Gtk hiding (get,set)
-- from hoodle-platform 
import           Data.Hoodle.Simple
-- import           Graphics.Hoodle.Render.Type
-- from this package
import           Hoodle.ModelAction.Select
import           Hoodle.Script.Hook
import           Hoodle.Type.Event 
import           Hoodle.Type.HoodleState 
--

-- | 
updateClipboard :: HoodleState -> [Item] -> IO HoodleState 
updateClipboard xstate itms 
  | null itms = return xstate
  | otherwise = do 
    let ui = view gtkUIManager xstate
    hdltag <- atomNew "hoodle"
    -- tgttag <- atomNew "Stroke"
    -- seltag <- atomNew "Stroke"
    clipbd <- clipboardGet hdltag
    let bstr = C8.unpack . B64.encode . Se.encode $ itms 
    clipboardSetText clipbd bstr
    togglePaste ui True 
    case (view hookSet xstate) of 
      Nothing -> return () 
      Just hset -> case afterUpdateClipboardHook hset of 
                     Nothing -> return () 
                     Just uchook -> liftIO $ uchook itms 
    return xstate



-- |
callback4Clip :: (MyEvent -> IO ()) -> Maybe String -> IO ()
callback4Clip callbk Nothing = callbk (GotClipboardContent Nothing)
callback4Clip callbk (Just str) = do
    let r = do let bstr = C8.pack str 
               bstr' <- B64.decode bstr
               Se.decode bstr' 
    case r of 
      Left _err -> callbk (GotClipboardContent Nothing)
      Right cnt -> callbk (GotClipboardContent (Just cnt))
