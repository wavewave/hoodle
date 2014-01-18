{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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

import           Control.Monad ((<=<),guard)
import           Control.Monad.Trans.Either
import           Data.Aeson as A
import           Data.Aeson.Encode
import           Data.Aeson.Encode.Pretty
import qualified Data.Attoparsec as AP
import           Data.Attoparsec.Number
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.Strict.Tuple
import qualified Data.Text as T
import           Data.UUID.V4
import           Data.Vector hiding (map,head,null,(++),take)
import           Control.Monad.Trans (liftIO)
import           System.Directory
import           System.FilePath
import           System.Process
-- 
import           Data.Hoodle.Simple
--
import           Hoodle.Coroutine.Dialog
import           Hoodle.Coroutine.Minibuffer
import           Hoodle.Type.Coroutine
-- 
import           Prelude hiding (fst,snd)

getArray :: (Monad m) => Value -> EitherT String m (Vector Value)
getArray (Array v) = right v
getArray _ = left "Not an array"

getArrayVal :: (Monad m) => Int -> Value -> EitherT String m Value
getArrayVal n v = getArray v >>= \vs -> 
                    maybe (left (show n ++ " is out of array")) right (vs !? n) 

handwritingRecognitionTest :: MainCoroutine ()
handwritingRecognitionTest = do
  liftIO $ putStrLn "handwriting recognition test here"
  r <- minibufDialog "test handwriting recognition"
  case r of 
    Left err -> liftIO $ putStrLn (show err) 
    Right strks -> do 
      uuid <- liftIO $ nextRandom
      tdir <- liftIO getTemporaryDirectory 
      let bstr = (encode . mkAesonInk) strks
      let fp = tdir </> show uuid <.> "json"
      liftIO $ LB.writeFile fp bstr
      r <- liftIO $ readProcess "curl" ["-X", "POST", "-H", "Content-Type: application/json ", "--data-ascii", "@"++fp, "https://inputtools.google.com/request?itc=en-t-i0-handwrit&app=chext" ] ""
      let ev0 = AP.parseOnly json (B.pack r)  
      liftIO $ print ev0
      r <- runEitherT $ do  
             v0 <- hoistEither (AP.parseOnly json (B.pack r))
             getArrayVal 0 v0 >>= \succstr -> 
               guard (succstr == (String "SUCCESS"))
             v4 <-  (getArray <=< getArrayVal 1 <=< getArrayVal 0 <=< getArrayVal 1) v0 
             let f (String v) = Just v
                 f _ = Nothing
             (return . map T.unpack . mapMaybe f . toList) v4
      case r of 
        Left err -> liftIO $ putStrLn err
        Right lst -> okMessageBox (unlines lst)
{-
      case ev0 of 
        Left _ -> return ()
        Right v0 -> case v0 of 
          Array v1 -> case v1 ! 0 of
            String "SUCCESS" -> do 
              liftIO (print (v1 ! 1))
              case v1 ! 1 of
                Array v2 -> case (v2 ! 0) of
                  Array v3 -> case v3 ! 1 of
                    Array v4 -> let f (String v) = Just v
                                    f _ = Nothing
                                    results = (map T.unpack . mapMaybe f . toList) v4
                                in okMessageBox (unlines results)
                      
                      
                    _ -> return ()
                  _ -> return ()
 --                  case v2 ! 1 of
   --             Array v3 -> 
                _ -> return ()
            _ -> return ()

          _ -> return ()
-}

mkAesonInk :: [Stroke] -> Value
mkAesonInk strks = 
    let strks_value = (Array . fromList . map mkAesonStroke) strks 
        hm0 = HM.insert "writing_area_width" (Number (I 500))
            . HM.insert "writing_area_height" (Number (I 50))
            $ HM.empty
        
        hm1 = HM.insert "writing_guide" (Object hm0) 
            . HM.insert "pre_context" (A.String "")
            . HM.insert "max_num_results" (Number (I 10))
            . HM.insert "max_completions" (Number (I 0))
            . HM.insert "ink" strks_value 
            $ HM.empty             
        hm2 = HM.insert "feedback" (A.String "∅[deleted]")
            . HM.insert "select_type" (A.String "deleted")
            $ HM.empty
        hm3 = HM.insert "app_version" (Number (D 0.4))
            . HM.insert "api_level" (A.String "537.36")
            . HM.insert "device" "hoodle"
            . HM.insert "input_type" (Number (I 0))
            . HM.insert "options" (A.String "enable_pre_space")
            . HM.insert "requests" (Array (fromList [Object hm1, Object hm2]))
            $ HM.empty
    in Object hm3
  
              
mkAesonStroke :: Stroke -> Value 
mkAesonStroke Stroke {..} = 
    let xs = map (Number . I . (floor :: Double -> Integer) . fst) stroke_data
        ys = map (Number . I . (floor :: Double -> Integer) . snd) stroke_data
    in Array (fromList [Array (fromList xs), Array (fromList ys)])
