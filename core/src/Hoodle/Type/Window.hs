module Hoodle.Type.Window where

import Hoodle.Type.Canvas

-- |
data WindowConfig
  = Node CanvasId
  | HSplit WindowConfig WindowConfig
  | VSplit WindowConfig WindowConfig
  deriving (Show, Eq)

data SplitType = SplitHorizontal | SplitVertical
  deriving (Show)

-- | split window in the place of cidold
splitWindow ::
  -- | old window
  CanvasId ->
  -- | new additional window
  (CanvasId, SplitType) ->
  -- | old WindowConfig
  WindowConfig ->
  -- | new WindowConfig
  Either WindowConfig WindowConfig
splitWindow cidold (cidnew, stype) (Node cid) =
  if cid == cidold
    then case stype of
      SplitHorizontal -> Right (HSplit (Node cid) (Node cidnew))
      SplitVertical -> Right (VSplit (Node cid) (Node cidnew))
    else Left (Node cid)
splitWindow cidold (cidnew, stype) (HSplit wconf1 wconf2) =
  let r1 = splitWindow cidold (cidnew, stype) wconf1
      r2 = splitWindow cidold (cidnew, stype) wconf2
   in case (r1, r2) of
        (Left nwconf1, Left nwconf2) -> Left (HSplit nwconf1 nwconf2)
        (Left nwconf1, Right nwconf2) -> Right (HSplit nwconf1 nwconf2)
        (Right nwconf1, Left nwconf2) -> Right (HSplit nwconf1 nwconf2)
        (Right _, Right _) -> error "such case cannot happen in splitWindow"
splitWindow cidold (cidnew, stype) (VSplit wconf1 wconf2) =
  let r1 = splitWindow cidold (cidnew, stype) wconf1
      r2 = splitWindow cidold (cidnew, stype) wconf2
   in case (r1, r2) of
        (Left nwconf1, Left nwconf2) -> Left (VSplit nwconf1 nwconf2)
        (Left nwconf1, Right nwconf2) -> Right (VSplit nwconf1 nwconf2)
        (Right nwconf1, Left nwconf2) -> Right (VSplit nwconf1 nwconf2)
        (Right _, Right _) -> error "such case cannot happen in splitWindow"

removeWindow ::
  -- | canvas id
  CanvasId ->
  WindowConfig ->
  Either WindowConfig (Maybe WindowConfig)
removeWindow cid (Node cid') =
  if cid == cid'
    then Right Nothing
    else Left (Node cid')
removeWindow cid (HSplit wconf1 wconf2) =
  let r1 = removeWindow cid wconf1
      r2 = removeWindow cid wconf2
   in case (r1, r2) of
        (Left nwconf1, Left nwconf2) -> Left (HSplit nwconf1 nwconf2)
        (Left nwconf1, Right mnwconf2) ->
          case mnwconf2 of
            Just nwconf2 -> Right (Just (HSplit nwconf1 nwconf2))
            Nothing -> Right (Just nwconf1)
        (Right mnwconf1, Left nwconf2) ->
          case mnwconf1 of
            Just nwconf1 -> Right (Just (HSplit nwconf1 nwconf2))
            Nothing -> Right (Just nwconf2)
        (Right _, Right _) -> error "such case cannot happen in removeWindow"
removeWindow cid (VSplit wconf1 wconf2) =
  let r1 = removeWindow cid wconf1
      r2 = removeWindow cid wconf2
   in case (r1, r2) of
        (Left nwconf1, Left nwconf2) -> Left (VSplit nwconf1 nwconf2)
        (Left nwconf1, Right mnwconf2) ->
          case mnwconf2 of
            Just nwconf2 -> Right (Just (VSplit nwconf1 nwconf2))
            Nothing -> Right (Just nwconf1)
        (Right mnwconf1, Left nwconf2) ->
          case mnwconf1 of
            Just nwconf1 -> Right (Just (VSplit nwconf1 nwconf2))
            Nothing -> Right (Just nwconf2)
        (Right _, Right _) -> error "such case cannot happen in removeWindow"
