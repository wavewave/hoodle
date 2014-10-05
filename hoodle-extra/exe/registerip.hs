{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.Char8 as B
import           Data.Monoid
import qualified Network.HTTP.Conduit as N
import           Network.Info
import           System.Environment
--
import Hoodle.Manage.Connect

main :: IO ()
main = do 
  args <- getArgs
  let ipfind = do 
        let ipv4num (IPv4 x) = x 
            ismacnull (MAC a b c d e f) = a == 0 && b == 0 && c == 0 
                                          && d == 0 && e == 0 && f == 0 
        ifcs <- getNetworkInterfaces
        let ifcs2 = Prelude.filter (not . ismacnull . mac) 
                    . Prelude.filter (((/=) 0) . ipv4num . ipv4 ) $ ifcs
        return (if Prelude.null ifcs2 then "127.0.0.1" else (show . ipv4 . head) ifcs2) 
  ip <- ipfind 
  registerip (args !! 0) (args !! 1) (args !! 2) ip

registerip :: String -> String -> String -> String -> IO ()
registerip url idee pwd ip = do 
    hubwork url idee pwd $ \ck -> do 
        let uuid = "c3d45ece-b57a-4c61-9781-0ed7ebddf020"
            bstr = "\"" <> B.pack ip <> "\"" 
        print bstr 
        request'' <- N.parseUrl (url <> "/device/" <> uuid )

        let requesttask = request'' 
              { N.method = "PUT"
              , N.requestHeaders = ("Content-Type", "application/json") 
                                   : ("Cookie",ck) 
                                   : N.requestHeaders request''
              , N.requestBody = N.RequestBodyBS bstr
              }
        runResourceT $ N.withManager $ \manager -> do  
          response <- N.http requesttask manager
          content <- N.responseBody response $$+- CL.consume 
          liftIO $ B.putStrLn (mconcat content)
