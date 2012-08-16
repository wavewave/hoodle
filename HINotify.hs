
------------------------------
-- | handling file system event
------------------------------

module HINotify where 

import           System.INotify 
-- 
import qualified Driver as D

watchFile :: INotify -> FilePath -> (D.Event -> IO ()) -> IO WatchDescriptor 
watchFile inotify fp handler = do 
  let action :: Event -> IO () 
      action (Accessed _ _) = handler (D.Message "Accessed")
      action (Modified _ _) = handler (D.Message "Modified")
      action (Closed _ _ _) = handler (D.Message "Closed") 
      action (Opened _ _) = handler (D.Message "Opened")
      action (MovedOut _ _ _) = handler (D.Message "MovedOut")
      action (MovedIn _ _ _ ) = handler (D.Message "MovedIn")
      action (MovedSelf _) = handler (D.Message "MovedSelf")
      action (Created _ _ ) = handler (D.Message "Created")
      action (Deleted _ _ ) = handler (D.Message "Deleted") 
      action (DeletedSelf) = handler (D.Message "DeletedSelf")
      action (Unmounted) = handler (D.Message "Unmounted") 
      action (QOverflow) = handler (D.Message "QOverflow")
      action (Ignored) = handler (D.Message "Ignored")
      action (Unknown _) = handler (D.Message "Unknown")
  addWatch inotify [AllEvents] fp action
  

