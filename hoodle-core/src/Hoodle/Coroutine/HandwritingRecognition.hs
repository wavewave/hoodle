{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.HandwritingRecognition
-- Copyright   : (c) 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.HandwritingRecognition where

import           Control.Lens (view,_1,_2)
import           Control.Monad ((<=<),guard,when)
import           Control.Monad.Trans (liftIO)
import           Control.Monad.Trans.Either
import           Data.Aeson as A
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Foldable (mapM_)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L (lookup)
import           Data.Maybe
import           Data.Strict.Tuple
import qualified Data.Text as T
import           Data.Traversable (forM)
import           Data.UUID.V4
import           Data.Vector hiding (map,head,null,(++),take,modify,mapM_,zip,forM)
import qualified Graphics.UI.Gtk as Gtk
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process
-- 
import           Data.Hoodle.Simple
-- from this package
import           Hoodle.Coroutine.Draw (waitSomeEvent)
import           Hoodle.Coroutine.Minibuffer
import           Hoodle.Type.Coroutine
import           Hoodle.Type.Event
import           Hoodle.Util
-- 
import           Prelude hiding (fst,snd,mapM_)

getArray :: (Monad m) => Value -> EitherT String m (Vector Value)
getArray (Array v) = right v
getArray _ = left "Not an array"

getArrayVal :: (Monad m) => Int -> Value -> EitherT String m Value
getArrayVal n v = getArray v >>= \vs -> 
                    maybe (left (show n ++ " is out of array")) right (vs !? n) 

handwritingRecognitionDialog :: MainCoroutine (Maybe (Bool,T.Text))
handwritingRecognitionDialog = do
  r <- minibufDialog "test handwriting recognition"
  case r of 
    Left err -> liftIO $ putStrLn (show err) >> return Nothing 
    Right strks -> do 
      uuid <- liftIO $ nextRandom
      tdir <- liftIO getTemporaryDirectory 
      let bstr = (encode . mkAesonInk) strks
      let fp = tdir </> show uuid <.> "json"
      liftIO $ LB.writeFile fp bstr
      (excode,gresult,gerror) <- liftIO $ readProcessWithExitCode "curl" ["-X", "POST", "-H", "Content-Type: application/json ", "--data-ascii", "@"++fp, "http://inputtools.google.com/request?itc=en-t-i0-handwrit" ] ""
      case excode of 
        ExitSuccess -> do 
          r_parse <- runEitherT $ do  
            v0 <- hoistEither (AP.parseOnly json (B.pack gresult))
            getArrayVal 0 v0 >>= \succstr -> 
              guard (succstr == (String "SUCCESS"))
            v4 <-(getArray <=< getArrayVal 1 
                  <=< getArrayVal 0 <=< getArrayVal 1) v0 
            let f (String v) = Just v
                f _ = Nothing
            (return . mapMaybe f . toList) v4
      
          case r_parse of 
            Left err -> msgShout ("handwritingRecognitionDialog: " ++ err) >> return Nothing 
            Right lst -> showRecogTextDialog lst 
        _ -> msgShout ("handwritingRecognitionDialog: " ++ gerror) >> return Nothing 



showRecogTextDialog :: [T.Text] -> MainCoroutine (Maybe (Bool,T.Text))
showRecogTextDialog txts = do 
    doIOaction action
    >> waitSomeEvent (\case OkCancel _ -> True 
                            GotRecogResult _ _ -> True
                            _ -> False)
    >>= \case OkCancel _ -> return Nothing
              GotRecogResult b txt -> return (Just (b,txt))
              _ -> return Nothing
  where 
    action = \evhandler -> do 
               dialog <- Gtk.dialogNew
#ifdef GTK3
               upper <- fmap Gtk.castToContainer (Gtk.dialogGetContentArea dialog)
               vbox <- Gtk.vBoxNew False 0        
               Gtk.containerAdd upper vbox
#else
               vbox <- Gtk.dialogGetUpper dialog
#endif
               let txtlst' = zip [1..] txts
               txtlst <- forM txtlst' $ \(n,txt) -> do
                 let str = T.unpack txt 
                 homedir <- getHomeDirectory
                 let hoodled = homedir </> ".hoodle.d"
                     hoodletdir = hoodled </> "hoodlet"
                 b <- doesDirectoryExist hoodletdir 
                 b2 <- if not b 
                         then return False   
                         else doesFileExist (hoodletdir </> str <.> "hdlt")      
                 return (n,(b2,txt))
               mapM_ (addOneTextBox evhandler dialog vbox) txtlst  
               _btnCancel <- Gtk.dialogAddButton dialog ("Cancel" :: String) Gtk.ResponseCancel
               Gtk.widgetShowAll dialog
               res <- Gtk.dialogRun dialog
               Gtk.widgetDestroy dialog
               case res of 
                 Gtk.ResponseUser n -> case L.lookup n txtlst of
                                     Nothing -> return (UsrEv (OkCancel False))
                                     Just (b,txt) -> return (UsrEv (GotRecogResult b txt)) 
                 _ -> return (UsrEv (OkCancel False))
                 
addOneTextBox :: (AllEvent -> IO ()) -> Gtk.Dialog -> Gtk.VBox 
              -> (Int,(Bool,T.Text)) -> IO ()
addOneTextBox _evhandler dialog vbox (n,(b,txt)) = do
  btn <- Gtk.buttonNewWithLabel (T.unpack txt)
  when b $ do 
    Gtk.widgetModifyBg btn Gtk.StateNormal (Gtk.Color 60000 60000 30000)
    Gtk.widgetModifyBg btn Gtk.StatePrelight (Gtk.Color 63000 63000 40000)
    Gtk.widgetModifyBg btn Gtk.StateActive (Gtk.Color 45000 45000 18000)
  btn `Gtk.on` Gtk.buttonPressEvent $ Gtk.tryEvent $ do
    liftIO $ Gtk.dialogResponse dialog (Gtk.ResponseUser n)
  Gtk.boxPackStart vbox btn Gtk.PackNatural 0 

mkAesonInk :: [Stroke] -> Value
mkAesonInk strks = 
    let strks_value = (Array . fromList . map mkAesonStroke) strks 
        hm0 = HM.insert "writing_area_width" (Number (fromInteger 500))
            . HM.insert "writing_area_height" (Number (fromInteger 50))
            $ HM.empty
        
        hm1 = HM.insert "writing_guide" (Object hm0) 
            . HM.insert "pre_context" (A.String "")
            . HM.insert "max_num_results" (Number (fromInteger 10))
            . HM.insert "max_completions" (Number (fromInteger 0))
            . HM.insert "ink" strks_value 
            $ HM.empty             
        hm2 = HM.insert "feedback" (A.String "∅[deleted]")
            . HM.insert "select_type" (A.String "deleted")
            $ HM.empty
        hm3 = HM.insert "app_version" (Number 0.4)
            . HM.insert "api_level" (A.String "537.36")
            . HM.insert "device" "hoodle"
            . HM.insert "input_type" (Number (fromInteger 0))
            . HM.insert "options" (A.String "enable_pre_space")
            . HM.insert "requests" (Array (fromList [Object hm1, Object hm2]))
            $ HM.empty
    in Object hm3
  
              
mkAesonStroke :: Stroke -> Value 
mkAesonStroke Stroke {..} = 
    let xs = map (Number . fromInteger . (floor :: Double -> Integer) . fst) stroke_data
        ys = map (Number . fromInteger . (floor :: Double -> Integer) . snd) stroke_data
    in Array (fromList [Array (fromList xs), Array (fromList ys)])
mkAesonStroke VWStroke {..} = 
    let xs = map (Number . fromInteger . (floor :: Double -> Integer) . view _1) stroke_vwdata
        ys = map (Number . fromInteger . (floor :: Double -> Integer) . view _2) stroke_vwdata
    in Array (fromList [Array (fromList xs), Array (fromList ys)])
