{-# LANGUAGE OverloadedStrings #-}

module Hoodle.Manage.Connect where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as B
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Foldable as F
import qualified Data.Traversable as Tr
import qualified Data.List as DL 
import           Data.Monoid
import qualified Network.HTTP.Conduit as N


hubwork :: String -> String -> String -> (B.ByteString -> IO a) -> IO (Maybe a)
hubwork url idee pwd act = do           
    request' <- N.parseUrl (url <> "/auth/page/hashdb/login")
    let crstr = B.pack ("username=" ++ idee ++ "&password=" ++ pwd)
        requestauth = request' 
          { N.method = "POST" 
          , N.requestHeaders = 
              ("Content-Type","application/x-www-form-urlencoded") 
              : N.requestHeaders request'   
          , N.requestBody = N.RequestBodyBS crstr
          } 
    mck <- runResourceT $ N.withManager $ \manager -> do  
             response <- N.http requestauth manager
             return (DL.lookup "Set-Cookie" (N.responseHeaders response))
    Tr.mapM act mck


{-
do 
        request'' <- N.parseUrl (url <> "/dumpdb")
        let requesttask = request'' 
              { N.method = "GET"
              , N.requestHeaders = ("Content-Type", "application/json") 
                                   : ("Cookie",ck) 
                                   : N.requestHeaders request''
              }
        runResourceT $ N.withManager $ \manager -> do  
          response <- N.http requesttask manager
          content <- N.responseBody response $$+- CL.consume 
          liftIO $ B.putStrLn (mconcat content)
-}
