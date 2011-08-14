module Text.Xournal.Parse.Enumerator where

import Text.XML.Enumerator.Parse
import qualified Data.ByteString as S
import Data.Enumerator
import qualified Data.Enumerator as E
import Control.Monad.IO.Class


parseXojFile :: FilePath -> IO ()
parseXojFile fn = do 
    x <- S.readFile fn
    run_ $ enumList 1 [x] $$ joinI $ parseBytes decodeEntities $$ iterPrint
  where
    iterPrint = do
        x <- E.head
        case x of
            Nothing -> return ()
            Just y -> liftIO (print y) >> iterPrint


