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
import           Control.Lens (view)
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import System.Directory
import System.FilePath (makeRelative)
--
import Hoodle.Coroutine.HubInternal
import Hoodle.Coroutine.Dialog
import Hoodle.Script.Hook
import Hoodle.Type.Coroutine
import Hoodle.Type.Hub
import Hoodle.Type.HoodleState
--

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
                     -- liftIO $ print (hinfo,relfp)
                     lift (uploadWork (canfp,relfp) hinfo)
              case r of 
                Nothing -> okMessageBox "upload not successful" >> return ()
                Just _ -> return ()  

