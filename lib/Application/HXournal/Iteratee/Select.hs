module Application.HXournal.Iteratee.Select where

import Graphics.UI.Gtk hiding (get,set)

import Application.HXournal.Type.Event 

import Application.HXournal.Type.Coroutine
import Application.HXournal.Type.Canvas
import Application.HXournal.Type.XournalState
import Application.HXournal.Device

import Application.HXournal.Iteratee.Draw

import qualified Data.Map as M

import Control.Monad.Trans
import qualified Control.Monad.State as St
import Control.Monad.Coroutine.SuspensionFunctors

import Control.Category
import Data.Label
import Prelude hiding ((.), id)

selectRectStart :: CanvasId -> PointerCoord -> Iteratee MyEvent XournalStateIO () 
selectRectStart cid pcoord = do    
    liftIO $ putStrLn "selectRectStart"
    ev <- await 
    case ev of 
      PenDown cid pcoord -> selectRectProcess cid pcoord  
      _ -> return ()
        
selectRectProcess :: CanvasId -> PointerCoord -> Iteratee MyEvent XournalStateIO () 
selectRectProcess cid pcoord = do    
    ev <- await 
    liftIO $ putStrLn "selectRectProcess"
    case ev of 
      PenUp cid' pcoord' -> return ()
      _ -> selectRectProcess cid pcoord 

