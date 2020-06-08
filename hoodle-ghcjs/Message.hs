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

serialize :: C2SMsg -> Text
serialize (NewStroke payload) = T.cons 'N' $ T.pack (show payload)
serialize (SyncRequest range) = T.cons 'S' $ T.pack (show range)


deserialize :: Text -> C2SMsg
deserialize t =
  let c = T.head t
  in case c of
       'N' -> NewStroke $ read $ T.unpack $ T.tail t
       'S' -> SyncRequest $ read $ T.unpack $ T.tail t
       _ -> error "cannot parse"
