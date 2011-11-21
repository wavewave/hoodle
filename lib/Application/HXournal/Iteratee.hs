{-# LANGUAGE OverloadedStrings #-}

module Application.HXournal.Iteratee where 

-- import Prelude hiding (uncurry)
import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.State
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.IO.Class

import qualified Data.ByteString.Lazy as L

import Application.HXournal.Type
import Application.HXournal.Util
import Application.HXournal.Draw
import Application.HXournal.Coroutine
import Application.HXournal.Builder

import Text.Xournal.Type 

import Graphics.UI.Gtk hiding (get)

import Data.Foldable (toList)
import Data.Sequence hiding (length,drop,take)
import Data.Strict.Tuple hiding (uncurry)

import Application.HXournal.Device

connPenMove :: (WidgetClass w) => w -> Iteratee MyEvent XournalStateIO (ConnectId w) 
connPenMove c = do 
  callbk <- lift $ callback <$> get 
  dev <- lift $ device <$> get 
  liftIO (c `on` motionNotifyEvent $ tryEvent $ do 
             p <- getPointer dev
             liftIO (callbk (PenMove p)))

connPenUp :: (WidgetClass w) => w -> Iteratee MyEvent XournalStateIO (ConnectId w) 
connPenUp c = do 
  callbk <- lift $ callback <$> get 
  dev <- lift $ device <$> get 
  liftIO (c `on` buttonReleaseEvent $ tryEvent $ do 
             p <- getPointer dev
             liftIO (callbk (PenMove p)))

iter :: Iteratee MyEvent XournalStateIO () 
iter = do liftIO (putStrLn "I am waiting first result") 
          sequence_ (repeat eventProcess)
          return ()

changePage :: (Int -> Int) -> Iteratee MyEvent XournalStateIO () 
changePage modifyfn = do 
  xstate <- lift get 

  let totalnumofpages = (length . xoj_pages) (xoj xstate)
      oldpage = currpage xstate
  let newpage | modifyfn oldpage >= totalnumofpages = totalnumofpages - 1
              | modifyfn oldpage < 0  = 0 
              | otherwise = modifyfn oldpage 
  lift (put (xstate { currpage = newpage}))
  liftIO (updateCanvas <$> darea <*> xoj <*> pure newpage $ xstate)
  -- liftIO . putStrLn $ "changing " ++ show oldpage ++ " to " ++ show newpage

eventProcess :: Iteratee MyEvent XournalStateIO ()
eventProcess = do 
  r1 <- await 
  case r1 of 
    ButtonLeft -> changePage (\x->x-1)
    ButtonRight -> changePage (+1)
    ButtonRefresh -> do 
      xstate <- lift get
      liftIO (updateCanvas <$> darea <*> xoj <*> currpage $ xstate)
      liftIO . putStrLn $ "refresh"
    ButtonQuit -> do  
      liftIO . putStrLn $ "quit"
    MenuSave -> do 
      xojcontent <- lift ( xoj <$> get )  
      liftIO $ L.writeFile "mytest.xoj" $ builder xojcontent
    PenDown pcoord -> do 
      canvas <- lift ( darea <$> get )  
      win <- liftIO $ widgetGetDrawWindow canvas
      pagenum <- lift (currpage <$> get )
      page <- lift ( (!!pagenum) . xoj_pages . xoj <$> get ) 
      geometry <- liftIO (getCanvasPageGeometry canvas page)
      let (x,y) = device2pageCoord geometry pcoord 
      -- liftIO . putStrLn $ "down " ++ show (x,y)
      -- liftIO $ penMoveTo canvas (x,y)
      connidup <- connPenUp canvas      
      connidmove <- connPenMove canvas
      pdraw <- penProcess win geometry connidmove connidup (empty |> (x,y)) (x,y) 
      xstate <- lift get 
      let currxoj = xoj xstate
          pgnum = currpage xstate 
      let newxoj = addPDraw currxoj pgnum pdraw
      lift $ put (xstate { xoj = newxoj }) 
      return ()
      -- liftIO (print pdraw) 
    _ -> defaultEventProcess r1

penProcess :: DrawWindow 
           -> CanvasPageGeometry
           -> ConnectId DrawingArea -> ConnectId DrawingArea 
           -> Seq (Double,Double) -> (Double,Double) 
           -> Iteratee MyEvent XournalStateIO (Seq (Double,Double))
penProcess win cpg connidmove connidup pdraw (x0,y0) = do 
  r <- await 
  case r of 
    PenMove pcoord -> do 
      canvas <- lift ( darea <$> get )
      let (x,y) = device2pageCoord cpg pcoord
      liftIO $ renderWithDrawable win $ drawSegment cpg 1.0 (0,0,0,1) (x0,y0) (x,y)
      penProcess win cpg connidmove connidup (pdraw |> (x,y)) (x,y) 
    PenUp pcoord -> do 
      canvas <- lift ( darea <$> get )
      let (x,y) = device2pageCoord cpg pcoord 
      liftIO $ signalDisconnect connidmove
      liftIO $ signalDisconnect connidup
      return (pdraw |> (x,y)) 
    other -> do
      defaultEventProcess other        
      penProcess win cpg connidmove connidup pdraw (x0,y0) 

defaultEventProcess :: MyEvent -> Iteratee MyEvent XournalStateIO () 
defaultEventProcess UpdateCanvas = do 
  xstate <- lift get
  liftIO (updateCanvas <$> darea <*> xoj <*> currpage $ xstate )
defaultEventProcess _ = return ()
  
addPDraw :: Xournal -> Int -> Seq (Double,Double) -> Xournal
addPDraw xoj pgnum pdraw = 
  let pagesbefore = take pgnum $ xoj_pages xoj  
      pagesafter  = drop (pgnum+1) $ xoj_pages xoj
      currpage = ((!!pgnum).xoj_pages) xoj 
      currlayer = head (page_layers currpage)
      otherlayers = tail (page_layers currpage)
      newstroke = Stroke { stroke_tool = "pen" 
                         , stroke_color = "black"   
                         , stroke_width = 1.0 
                         , stroke_data = map (uncurry (:!:)) . toList $ pdraw
                         } 
      newlayer = currlayer {layer_strokes = layer_strokes currlayer ++ [newstroke]}
      newpage = currpage {page_layers = newlayer : otherlayers }
      newxoj = xoj { xoj_pages =  pagesbefore ++ [newpage] ++ pagesafter }  
  in  newxoj










