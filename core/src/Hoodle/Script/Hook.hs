module Hoodle.Script.Hook where

import Data.Hoodle.Simple (Hoodle, Item)
import Graphics.Hoodle.Render.Type.Hoodle (RPage)

-- |
data Hook = Hook
  { saveAsHook :: Maybe (Hoodle -> IO ()),
    afterSaveHook :: Maybe (FilePath -> Hoodle -> IO ()),
    afterOpenHook :: Maybe (FilePath -> Hoodle -> IO ()),
    afterUpdateClipboardHook :: Maybe ([Item] -> IO ()),
    customContextMenuTitle :: Maybe String,
    customContextMenuHook :: Maybe ([Item] -> IO ()),
    customAutosavePage :: Maybe (RPage -> IO ()),
    fileNameSuggestionHook :: Maybe (IO String),
    recentFolderHook :: Maybe (IO FilePath),
    embedPredefinedImageHook :: Maybe (IO FilePath),
    embedPredefinedImage2Hook :: Maybe (IO FilePath),
    embedPredefinedImage3Hook :: Maybe (IO FilePath),
    lookupPathFromId :: Maybe (String -> IO (Maybe FilePath)),
    warningEmbedImageSize :: Maybe Integer,
    shrinkCmd4EmbedImage :: Maybe (Double -> FilePath -> FilePath -> IO ()),
    getIPaddress :: Maybe (IO String)
  }

defaultHook :: Hook
defaultHook =
  Hook
    { saveAsHook = Nothing,
      afterSaveHook = Nothing,
      afterOpenHook = Nothing,
      afterUpdateClipboardHook = Nothing,
      customContextMenuTitle = Nothing,
      customContextMenuHook = Nothing,
      customAutosavePage = Nothing,
      fileNameSuggestionHook = Nothing,
      recentFolderHook = Nothing,
      embedPredefinedImageHook = Nothing,
      embedPredefinedImage2Hook = Nothing,
      embedPredefinedImage3Hook = Nothing,
      lookupPathFromId = Nothing,
      warningEmbedImageSize = Nothing,
      shrinkCmd4EmbedImage = Nothing,
      getIPaddress = Nothing
    }
