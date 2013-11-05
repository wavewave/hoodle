{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Type.Coroutine 
-- Copyright   : (c) 2011-2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Type.Coroutine where

-- from other packages 
import           Control.Applicative
import           Control.Concurrent
import           Control.Lens ((^.),(.~),(%~),view,set)

import           Control.Monad.Reader 
import           Control.Monad.State
import           Control.Monad.Trans.Either 
import           Data.Time.Clock
import           Data.Time.LocalTime
-- from hoodle-platform
import           Control.Monad.Trans.Crtn 
import           Control.Monad.Trans.Crtn.Object
import qualified Control.Monad.Trans.Crtn.Driver as D
import           Control.Monad.Trans.Crtn.Logger 
import           Control.Monad.Trans.Crtn.Queue 
import           Control.Monad.Trans.Crtn.World
-- from this package
import           Hoodle.Type.Canvas
import           Hoodle.Type.Event
import           Hoodle.Type.HoodleState 
import           Hoodle.Type.Widget
import           Hoodle.Util
-- 

-- |
data MainOp i o where 
  DoEvent :: MainOp AllEvent () 

doEvent :: (Monad m) => AllEvent -> CObjT MainOp m () 
doEvent ev = request (Arg DoEvent ev) >> return ()

-- |
type MainCoroutine = MainObjB 
                     
type MainObjB = SObjBT MainOp (EStT HoodleState WorldObjB)

-- | 
type MainObj = SObjT MainOp (EStT HoodleState WorldObjB)

-- | 
nextevent :: MainCoroutine UserEvent 
nextevent = do Arg DoEvent ev <- request (Res DoEvent ())
               case ev of
                 SysEv sev -> sysevent sev >> nextevent 
                 UsrEv uev -> return uev 

sysevent :: SystemEvent -> MainCoroutine () 
sysevent ClockUpdateEvent = do 
  utctime <- liftIO $ getCurrentTime 
  zone <- liftIO $ getCurrentTimeZone  
  let ltime = utcToLocalTime zone utctime 
      ltimeofday = localTimeOfDay ltime 
      (h,m,s) :: (Int,Int,Int) = 
        (,,) <$> (\x->todHour x `mod` 12) <*> todMin <*> (floor . todSec) 
        $ ltimeofday
  liftIO $ print (h,m,s)
  xst <- get 
  let cinfo = view currentCanvasInfo xst
      cwgts = view (unboxLens canvasWidgets) cinfo   
      nwgts = set (clockWidgetConfig.clockWidgetTime) (h,m,s) cwgts
      ncinfo = set (unboxLens canvasWidgets) nwgts cinfo
  put . set currentCanvasInfo ncinfo $ xst 
              
  when (view (widgetConfig.doesUseClockWidget) cwgts) $ do 
    let cid = getCurrentCanvasId xst
    modify (tempQueue %~ enqueue (Right (UsrEv (UpdateCanvasEfficient cid))))

    -- invalidateInBBox Nothing Efficient cid   
sysevent ev = liftIO $ print ev 

-- | 
type WorldObj = SObjT (WorldOp AllEvent DriverB) DriverB  

-- | 
type WorldObjB = SObjBT (WorldOp AllEvent DriverB) DriverB 

-- | 
world :: HoodleState -> MainObj () -> WorldObj ()
world xstate initmc = ReaderT staction  
  where 
    staction req = runStateT erract xstate >> return ()
      where erract = do r <- runEitherT (go initmc req) 
                        case r of 
                          Left e -> liftIO (errorlog (show e)) 
                          Right _r' -> return () 
    go :: MainObj() 
          -> Arg (WorldOp AllEvent DriverB) 
          -> EStT HoodleState WorldObjB () 
    go mcobj (Arg GiveEvent ev) = do 
      Right mcobj' <- liftM (fmap fst) (mcobj <==| doEvent ev)
      req <- lift . lift $ request (Res GiveEvent ())
      go mcobj' req  
    go mcobj (Arg FlushLog logobj) = do  
      logf <- (^. tempLog) <$> get  
      let msg = logf "" 
      if ((not.null) msg)
        then do 
          Right logobj' <- lift . lift . lift $ liftM (fmap fst) (logobj <==| writeLog msg)
          modify (tempLog .~ id)
          req <- lift . lift $ request (Res FlushLog logobj')
          go mcobj req 
        else do 
          req <- lift . lift $ request Ign 
          go mcobj req 
    go mcobj (Arg FlushQueue ()) = do 
      q <- (^. tempQueue) <$> get 
      let lst = fqueue q ++ reverse (bqueue q)
      modify (tempQueue .~ emptyQueue)
      req <- lift .  lift $ request (Res FlushQueue lst)
      go mcobj req 




-- | 
type Driver a = D.Driver AllEvent IO a -- SObjT MainOp IO a 

-- | 
type DriverB = SObjBT (D.DrvOp AllEvent) IO  

-- | 
type EventVar = MVar (Maybe (Driver ()))




-- | 
maybeError :: String -> Maybe a -> MainCoroutine a
maybeError str = maybe (lift . hoistEither . Left . Other $ str) return 


-- | 
doIOaction :: ((AllEvent -> IO ()) -> IO AllEvent) -> MainCoroutine ()
doIOaction action = modify (tempQueue %~ enqueue (mkIOaction action))



