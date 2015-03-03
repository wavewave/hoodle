import Text.Hoodle.Parse.Util

import Control.Lens
import Data.Aeson
import Data.Aeson.Encode
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 as BL
import Data.Hoodle.Simple

main = do
  withHoodle "fficxx_portal.hdl" $ \hdl -> 
    BL.putStrLn $ encode hdl
    -- BL.putStrLn $ encodePretty hdl
    -- print $ length (view pages hdl)


  return ()
