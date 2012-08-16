{-# LANGUAGE GADTs #-}

module FileObserver where

import System.FilePath 
--
import Coroutine 
import Object 

data FileEvent = FileOpen | FileClose
                 deriving (Show,Eq)

data FOOp i o where 
  WatchFile :: FOOp FilePath () 
  WaitFileEvent :: FOOp () FileEvent 
--   CheckGrasp :: Foop () FileStatus 


type FOInput = MethodInput FOOp 

type FileObserver m r = ServerObj FOOp m r 

type FileObserverClient m r = ClientObj FOOp m r 

-------------------------
-- file observer client primitive action 
-------------------------

-- |
watchFile :: (Monad m) => FilePath -> FileObserverClient m ()
watchFile fp = do request (Input WatchFile fp)
                  return () 


-- | 
waitFileEvent :: (Monad m) => FileObserverClient m FileEvent 
waitFileEvent = do r <- request (Input WaitFileEvent ())
                   case r of 
                     Output WaitFileEvent ev -> return ev 
                     _ -> error "waitFileEvent" -- Allow partiality here 

-- | 
fileObserver :: (Monad m) => FileObserver m () 
fileObserver req = do init 
                      go Nothing req  
  where init = return ()

        go :: Monad m => Maybe FilePath -> FileObserver m () 
        go mfp req = 
          case req of 
            Input WatchFile fp  -> do req' <- request (Output WatchFile ())
                                      go (Just fp) req'  
            Input WaitFileEvent () -> do 
                                         req' <- request (Output WaitFileEvent FileClose)
                                         go mfp req'