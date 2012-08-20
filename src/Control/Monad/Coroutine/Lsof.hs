module Control.Monad.Coroutine.Lsof where

import System.Directory 
import System.Exit 
import System.Process

data FileStatus = NotExist 
                | Grasped 
                | Free 
                  deriving (Show,Eq,Ord)

-- | check the file status of a file using lsof utility 
checkLsof :: FilePath -> IO FileStatus 
checkLsof fp = do 
  b <- doesFileExist fp 
  if b then do (_mhin,Just hout,_mherr,h) <- createProcess (shell $ "lsof " ++ fp) { std_out = CreatePipe }  
               excode <- waitForProcess h 
               case excode of 
                 ExitSuccess -> return Grasped 
                 _ -> return Free 
       else return NotExist 


