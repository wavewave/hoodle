-- common message type between server and client

module Message where

import Data.Text (Text)
import qualified Data.Text as T

data C2SMsg =
    NewStroke (Int, [(Double,Double)])
  | SyncRequest (Int,Int)

tag_NewStroke :: Char
tag_NewStroke = 'N'

tag_SyncRequest :: Char
tag_SyncRequest = 'S'

data S2CMsg =
    RegisterStroke (Int,Int) -- ^ (id,hash)

tag_RegisterStroke :: Char
tag_RegisterStroke = 'R'

class TextSerializable a where
  serialize :: a -> Text
  deserialize :: Text -> a

instance TextSerializable C2SMsg where
  serialize (NewStroke payload) = T.cons 'N' $ T.pack (show payload)
  serialize (SyncRequest range) = T.cons 'S' $ T.pack (show range)

  deserialize t =
    let c = T.head t
    in case c of
         'N' -> NewStroke $ read $ T.unpack $ T.tail t
         'S' -> SyncRequest $ read $ T.unpack $ T.tail t
         _ -> error "cannot parse"

instance TextSerializable S2CMsg where
  serialize (RegisterStroke payload) = T.cons 'R' $ T.pack (show payload)

  deserialize t =
    let c = T.head t
    in case c of
         'R' -> RegisterStroke $ read $ T.unpack $ T.tail t
         _ -> error "cannot parse"
