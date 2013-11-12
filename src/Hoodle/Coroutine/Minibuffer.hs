{-# LANGUAGE LambdaCase #-}

module Hoodle.Coroutine.Minibuffer where 

import           Control.Lens ((%~))
import           Control.Monad.State (modify)
import           Graphics.UI.Gtk hiding (get,set)
import           Graphics.Rendering.Cairo
-- 
import           Control.Monad.Trans.Crtn.Event
import           Control.Monad.Trans.Crtn.Queue (enqueue)
import           Data.Hoodle.Simple
--
import           Hoodle.Coroutine.Draw
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState


minibufDialog :: String -> MainCoroutine (Either () [Stroke])
minibufDialog msg = modify (tempQueue %~ enqueue action) 
                    >> waitSomeEvent (\case OkCancel b -> True 
                                            ChangeDialog -> True
                                            _ -> False)
                    >>= \case OkCancel b -> (return . Right) []  
                              ChangeDialog -> return (Left ())
                              _ -> return (Right [])
  where 
    action = mkIOaction $ 
               \_evhandler -> do 
                 dialog <- dialogNew 
                 cvs <- drawingAreaNew                           
                 cvs `on` sizeRequest $ return (Requisition 500 50)
                 cvs `on` exposeEvent $ tryEvent $ do
                   drawwdw <- liftIO $ widgetGetDrawWindow cvs                 
                   liftIO . renderWithDrawable drawwdw $ do
                     setSourceRGBA 0.95 0.85 0.5 1
                     rectangle 5 2 490 46
                     fill 
                     setSourceRGBA 0 0 0 1
                     setLineWidth 1.0
                     rectangle 5 2 490 46 
                     stroke
                 vbox <- dialogGetUpper dialog
                 boxPackStart vbox cvs PackNatural 0
                 btnOk <- dialogAddButton dialog "Ok" ResponseOk
                 btnCancel <- dialogAddButton dialog "Cancel" ResponseCancel
                 btnText <- dialogAddButton dialog "TextInput" (ResponseUser 1) 
                 widgetShowAll dialog
                 res <- dialogRun dialog 
                 widgetDestroy dialog 
                 case res of 
                   ResponseOk -> return (UsrEv (OkCancel True))
                   ResponseCancel -> return (UsrEv (OkCancel False))
                   ResponseUser 1 -> return (UsrEv ChangeDialog)
                   _ -> return (UsrEv (OkCancel False))
