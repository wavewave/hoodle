module Hoodle.Manage.Connect where

import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as B
import qualified Network.HTTP.Conduit as N

dbgetwork :: String -> String -> String -> IO ()
dbgetwork url idee pwd = do           
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
    case mck of 
      Nothing -> return()
      Just ck -> do 
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
