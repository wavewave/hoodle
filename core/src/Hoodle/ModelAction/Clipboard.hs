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
module Hoodle.ModelAction.Clipboard where

import Control.Lens (view)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import Data.Hoodle.Simple (Item)
import qualified Data.Serialize as Se
import qualified Graphics.UI.Gtk as Gtk
import Hoodle.ModelAction.Select (togglePaste)
import Hoodle.Script.Hook (afterUpdateClipboardHook)
import Hoodle.Type.Event
  ( AllEvent (UsrEv),
    UserEvent (GotClipboardContent),
  )
import Hoodle.Type.HoodleState
  ( HoodleState,
    gtkUIManager,
    hookSet,
  )

-- |
updateClipboard :: HoodleState -> [Item] -> IO HoodleState
updateClipboard xstate itms
  | null itms = return xstate
  | otherwise = do
    let ui = view gtkUIManager xstate
    hdltag <- Gtk.atomNew "hoodle"
    clipbd <- Gtk.clipboardGet hdltag
    let bstr = C8.unpack . B64.encode . Se.encode $ itms
    Gtk.clipboardSetText clipbd bstr
    togglePaste ui True
    case view hookSet xstate of
      Nothing -> return ()
      Just hset -> case afterUpdateClipboardHook hset of
        Nothing -> return ()
        Just uchook -> liftIO $ uchook itms
    return xstate

-- |
callback4Clip :: (AllEvent -> IO ()) -> Maybe String -> IO ()
callback4Clip callbk Nothing = callbk (UsrEv (GotClipboardContent Nothing))
callback4Clip callbk (Just str) = do
  let r = do
        let bstr = C8.pack str
        bstr' <- B64.decode bstr
        Se.decode bstr'
  case r of
    Left _err -> callbk (UsrEv (GotClipboardContent Nothing))
    Right cnt -> callbk (UsrEv (GotClipboardContent (Just cnt)))
