{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Message where

import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Binary as Bi
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Simple.TCP

data Message = Message { msgbody :: T.Text } deriving Show

instance Binary Message where
  put :: Message -> Put
  put Message {..} = put (TE.encodeUtf8 msgbody)
  
  get :: Get Message
  get = do bdy_bstr <- get
           let bdy = TE.decodeUtf8 bdy_bstr 
           return (Message bdy)

packAndSend :: (Bi.Binary a) => Socket -> a -> IO ()
packAndSend sock itm = do
    let bmsg = (L.toStrict . Bi.encode) itm 
        sz :: Bi.Word32 = fromIntegral (S.length bmsg)
        sz_bstr = (L.toStrict . Bi.encode) sz
    send sock sz_bstr
    send sock bmsg

recvAndUnpack :: (MonadIO m, Bi.Binary a) => Socket -> MaybeT m a
recvAndUnpack sock = do
    sz_bstr <- MaybeT . liftIO $ recv sock 4
    let sz :: Bi.Word32 = (Bi.decode . L.fromStrict) sz_bstr
    str <- MaybeT . liftIO $ recv sock (fromIntegral sz)
    return $ (Bi.decode . L.fromStrict) str



