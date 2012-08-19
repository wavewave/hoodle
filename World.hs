{-# LANGUAGE GADTs, NoMonomorphismRestriction #-}

----------------------------
-- | describe world object
----------------------------

module World where 

import Control.Monad.Reader
import Control.Monad.Trans 
-- 
import Coroutine
import Event 
import Object

data WorldOp i o where 
  GiveEvent :: WorldOp Event () 
  Render :: WorldOp () () 

-- | 
type World m r = ServerObj WorldOp m r  


-- | 
giveEvent :: (Monad m) => Event -> ClientObj WorldOp m () 
giveEvent ev = request (Input GiveEvent ev) >> return () 


-- | 
render :: (Monad m) => ClientObj WorldOp m () 
render = request (Input Render ()) >> return ()

-- | 
world :: (MonadIO m) => ServerObj WorldOp m () 
world = ReaderT (worldW "") 
  where 
    worldW str (Input Render ()) = do 
      liftIO $ putStrLn str 
      req <- request (Output Render ())
      worldW str req 
    worldW str (Input GiveEvent ev) = do 
      let str' = case ev of 
                   Message msg -> str ++ "\n" ++ msg
      req <- request (Output GiveEvent ())
      worldW str' req 


