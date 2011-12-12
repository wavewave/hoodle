module Application.HXournal.Type.Window where

import Application.HXournal.Type.Canvas

import Graphics.UI.Gtk hiding (get,set)

data WindowConfig = Node CanvasId 
                  | HSplit (Maybe HPaned) WindowConfig WindowConfig
                  | VSplit (Maybe VPaned) WindowConfig WindowConfig 

data SplitType = SplitHorizontal | SplitVertical 
               deriving (Show)

-- | split window in the place of cidold 

splitWindow :: CanvasId  -- ^ old window 
               -> (CanvasId, SplitType) -- ^ new additional window
               -> WindowConfig -- ^ old WindowConfig
               -> Either WindowConfig WindowConfig -- ^ new WindowConfig
splitWindow cidold (cidnew,stype) (Node cid) = 
  if cid == cidold 
    then case stype of 
           SplitHorizontal -> Right (HSplit Nothing (Node cid) (Node cidnew))
           SplitVertical -> Right (VSplit Nothing (Node cid) (Node cidnew))
    else Left (Node cid)
splitWindow cidold (cidnew,stype) (HSplit hpane wconf1 wconf2) =
  let r1 = splitWindow cidold (cidnew,stype) wconf1
      r2 = splitWindow cidold (cidnew,stype) wconf2
  in  case (r1,r2) of 
        (Left nwconf1, Left nwconf2) -> Left (HSplit hpane nwconf1 nwconf2)
        (Left nwconf1, Right nwconf2) -> Right (HSplit hpane nwconf1 nwconf2)
        (Right nwconf1, Left nwconf2) -> Right (HSplit hpane nwconf1 nwconf2)
        (Right _, Right _) -> error "such case cannot happen in splitWindow"
splitWindow cidold (cidnew,stype) (VSplit vpane wconf1 wconf2) =
  let r1 = splitWindow cidold (cidnew,stype) wconf1
      r2 = splitWindow cidold (cidnew,stype) wconf2
  in  case (r1,r2) of 
        (Left nwconf1, Left nwconf2) -> Left (VSplit vpane nwconf1 nwconf2)
        (Left nwconf1, Right nwconf2) -> Right (VSplit vpane nwconf1 nwconf2)
        (Right nwconf1, Left nwconf2) -> Right (VSplit vpane nwconf1 nwconf2)
        (Right _, Right _) -> error "such case cannot happen in splitWindow"
     

removeWindow :: CanvasId -- ^ canvas id  
               -> WindowConfig
               -> Either WindowConfig (Maybe WindowConfig)
removeWindow cid (Node cid') = 
  if cid == cid' 
    then Right Nothing
    else Left (Node cid')
removeWindow cid (HSplit hpane wconf1 wconf2) =
  let r1 = removeWindow cid wconf1
      r2 = removeWindow cid wconf2
  in  case (r1,r2) of 
        (Left nwconf1, Left nwconf2) -> Left (HSplit hpane nwconf1 nwconf2)
        (Left nwconf1, Right mnwconf2) -> 
          case mnwconf2 of 
            Just nwconf2 -> Right (Just (HSplit hpane nwconf1 nwconf2))
            Nothing -> Right (Just nwconf1)
        (Right mnwconf1, Left nwconf2) -> 
          case mnwconf1 of
            Just nwconf1 -> Right (Just (HSplit hpane nwconf1 nwconf2))
            Nothing -> Right (Just nwconf2)
        (Right _, Right _) -> error "such case cannot happen in removeWindow"
removeWindow cid (VSplit vpane wconf1 wconf2) =
  let r1 = removeWindow cid wconf1
      r2 = removeWindow cid wconf2
  in  case (r1,r2) of 
        (Left nwconf1, Left nwconf2) -> Left (VSplit vpane nwconf1 nwconf2)
        (Left nwconf1, Right mnwconf2) -> 
          case mnwconf2 of 
            Just nwconf2 -> Right (Just (VSplit vpane nwconf1 nwconf2))
            Nothing -> Right (Just nwconf1)
        (Right mnwconf1, Left nwconf2) -> 
          case mnwconf1 of
            Just nwconf1 -> Right (Just (VSplit vpane nwconf1 nwconf2))
            Nothing -> Right (Just nwconf2)
        (Right _, Right _) -> error "such case cannot happen in removeWindow"
               
