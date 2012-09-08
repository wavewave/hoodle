-----------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.Trans.Crtn.EventHandler
-- Copyright   : (c) 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Common event handling 
-- 
-----------------------------------------------------------------------------

module Control.Monad.Trans.Crtn.EventHandler where 
  
import Control.Concurrent.MVar 
import Control.Monad.State 
import Control.Monad.Error
-- from this package 
-- import Control.Monad.Trans.Crtn
import Control.Monad.Trans.Crtn.Event 
import Control.Monad.Trans.Crtn.Driver 
import Control.Monad.Trans.Crtn.Logger 

-- | 
eventHandler :: (Show e) => MVar (Maybe (Driver e IO ())) -> e -> IO ()
eventHandler evar ev = do 
    putStrLn " In eventhandler : " ++ show ev
    mnext <- takeMVar evar
    case mnext of 
      Nothing -> return () 
      Just drv -> do                
        (r,drv') <- eaction drv 
        putMVar evar (Just drv')
        case r of    
          Left err -> scribe (show err) -- >>= putMVar evar (Just drv') 
          Right Nothing -> return () -- putMVar evar (Just drv')  
          Right (Just (ActionOrder act)) -> do e <- act (eventHandler evar)
                                               eventHandler evar e 
  where eaction = runStateT (runErrorT $ fire ev) 
                  
                  
                  
                  --  >> lift get >>= return)  
          
{-          
          >>= either (\err -> scribe (show err) >> return drv) 
                               (\drv'
          
          
          
          return >>= putMVar evar . Just  
-}                
  
{-  
    drv <- takeMVar dref 
    eaction drv >>= either (\err -> scribe (show err) >> return drv) return >>= putMVar dref 
  where -- eaction :: Driver e IO () -> IO (Either (CrtnErr ()) (Driver e IO ()))
        eaction = evalStateT $ runErrorT $ fire ev >> lift get >>= return -}

