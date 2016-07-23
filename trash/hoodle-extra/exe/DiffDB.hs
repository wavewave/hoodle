{-# LANGUAGE OverloadedStrings #-}

module DiffDB where

import           Data.Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import           Data.Maybe (fromJust,mapMaybe)
import qualified Data.Text as T
-- 

data DiffType = Same 
              | RightNew (String,FilePath) 
              | LeftNew (String,FilePath) 
              | Conflict (String,FilePath) (String,FilePath)  
              deriving (Show,Eq)
                       
instance ToJSON DiffType where
  toJSON Same = object [ "type" .= String "same" ] 
  toJSON (RightNew (h,f)) = object [ "type" .= String "rightnew"
                                   , "righthash" .= toJSON h
                                   , "rightpath" .= toJSON f ]
  toJSON (LeftNew (h,f)) = object [ "type" .= String "leftnew"
                                  , "lefthash" .= toJSON h 
                                  , "leftpath" .= toJSON f ]
  toJSON (Conflict (h1,f1) (h2,f2))
    = object [ "type" .= String "conflict" 
             , "lefthash" .= toJSON h1
             , "leftpath" .= toJSON f1
             , "righthash" .= toJSON h2
             , "rightpath" .= toJSON f2 ]
      
instance FromJSON DiffType where
  parseJSON (Object v) = 
    let r = do String typ <- H.lookup "type" v 
               case typ of 
                 "same" -> return Same
                 "rightnew" -> do 
                   String h <- H.lookup "righthash" v
                   String f <- H.lookup "rightpath" v
                   return (RightNew (T.unpack h,T.unpack f)) 
                 "leftnew" -> do 
                   String h <- H.lookup "lefthash" v
                   String f <- H.lookup "leftpath" v
                   return (LeftNew (T.unpack h,T.unpack f))
                 "conflict" -> do 
                   String h1 <- H.lookup "lefthash" v
                   String f1 <- H.lookup "leftpath" v
                   String h2 <- H.lookup "righthash" v
                   String f2 <- H.lookup "rightpath" v
                   return (Conflict (T.unpack h1, T.unpack f1) (T.unpack h2, T.unpack f2))
                 _ -> Nothing 
    in maybe (fail "error in parsing difftype") return r 
  parseJSON _ = fail "error in parsing difftype"
checkdiff :: M.Map String (String,FilePath) 
          -> M.Map String (String,FilePath) 
          -> M.Map String DiffType
checkdiff olddb newdb = 
  let oks = M.keys olddb 
      nks = M.keys newdb
      ks = oks ++ nks
      compf k = case (M.lookup k olddb,M.lookup k newdb) of  
        (Nothing,Nothing) -> Nothing
        (Just v,Nothing) -> Just (k,LeftNew v)
        (Nothing,Just v) -> Just (k,RightNew v)
        (Just v,Just v') -> 
          if v == v' then Just (k,Same) else Just (k,Conflict v v')
  in (M.fromList . filter ((/=Same).snd) . mapMaybe compf) ks
