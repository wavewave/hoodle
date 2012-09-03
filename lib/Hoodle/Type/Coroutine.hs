-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Coroutine 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Type.Coroutine where

-- from other packages 
import Control.Concurrent 
import Control.Monad.Reader 
import Control.Monad.State
import Control.Monad.Trans.Free
import Data.IORef 
-- from hoodle-platform
import Control.Monad.Trans.Crtn 
-- from this package
import Hoodle.Type.Event
import Hoodle.Type.XournalState 
-- 

-- | 
-- type MainCoroutine a = CnsmT MyEvent XournalStateIO a 
type MainCoroutine a = CrtnT () MyEvent XournalStateIO a 

-- |
-- type Driver a = CnsmT MyEvent IO a 
type Driver a = SrvT MyEvent () IO a 


-- type TRef = IORef (Maybe (Driver ()))

type EventVar = MVar (Maybe (Driver ()))

-- | 
dispatch :: MyEvent -> Driver a -> IO (Either a (Driver a))
dispatch ev drv = do 
  x <- runFreeT (runReaderT drv ev) 
  case x of 
    Pure a -> return (Left a)  
    Free (Rqst () next') -> return (Right (ReaderT next'))


-- | 
nextevent :: MainCoroutine MyEvent 
nextevent = request ()  
