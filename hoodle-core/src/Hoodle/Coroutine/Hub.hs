{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Coroutine.Hub
-- Copyright   : (c) 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Coroutine.Hub where

import           Control.Applicative
import qualified Control.Exception as E
import           Control.Lens (view)
import           Control.Monad (unless)
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
-- import           Control.Monad.Trans.State
import           Data.Aeson as AE
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import Data.Monoid ((<>))
import Data.Text (Text,pack,unpack)
import Data.Text.Encoding (encodeUtf8,decodeUtf8)
import Data.Time.Calendar
import Data.Time.Clock
import Data.UUID.V4
import Network
import Network.Google.OAuth2 (formUrl, exchangeCode, refreshTokens,
                               OAuth2Client(..), OAuth2Tokens(..))
import Network.Google (makeRequest, doRequest)
import Network.HTTP.Conduit
import Network.HTTP.Types (methodPut)
import System.Directory
import System.Environment (getEnv)
import System.Exit    (ExitCode(..))
import System.FilePath ((</>),(<.>),makeRelative)
import System.Info (os)
import System.Process (system, rawSystem,readProcessWithExitCode)
--
-- import Data.Hoodle.Generic
import Data.Hoodle.Simple
import Graphics.Hoodle.Render.Type.Hoodle
import Text.Hoodle.Builder (builder)
--
import Hoodle.Coroutine.HubInternal
import Hoodle.Coroutine.Dialog
import Hoodle.Script.Hook
import Hoodle.Type.Coroutine
import Hoodle.Type.Hub
import Hoodle.Type.HoodleState
import Hoodle.Util

-- |
hubUpload :: MainCoroutine ()
hubUpload = do
    xst <- get
    uhdl <- view (unitHoodles.currentUnit) <$> get
    if not (view isSaved uhdl) 
      then 
        okMessageBox "hub action can be done only after saved" >> return ()
      else do r <- runMaybeT $ do 
                     hset <- (MaybeT . return) $ view hookSet xst
                     hinfo <- (MaybeT . return) (hubInfo hset)
                     let hdir = hubfileroot hinfo
                     fp <- (MaybeT . return) (view (hoodleFileControl.hoodleFileName) uhdl)
                     canfp <- liftIO $ canonicalizePath fp
                     let relfp = makeRelative hdir canfp

                     liftIO $ print hinfo
                     lift (uploadWork (canfp,relfp) hinfo)
              case r of 
                Nothing -> okMessageBox "upload not successful" >> return ()
                Just _ -> return ()  

