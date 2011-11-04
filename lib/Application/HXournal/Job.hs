{-# LANGUAGE ScopedTypeVariables #-}

module Application.HXournal.Job where

import Application.HXournal.GUI
import Application.HXournal.Iteratee 
import Application.HXournal.Type 
import Data.IORef
import Control.Monad.Coroutine
import Control.Monad.State

import Text.Xournal.Type
import Text.Xournal.Parse
import Text.Xournal.Predefined

import Text.StringTemplate
import Text.StringTemplate.Helpers

import Graphics.Xournal.Render
import Graphics.Rendering.Cairo

import System.Directory
import System.FilePath
import System.IO

import Data.List 

import Paths_hxournal



startJob :: FilePath -> IO () 
startJob fname = do 
  putStrLn "job started"
  startGUI fname

startMakeSVG :: FilePath -> IO () 
startMakeSVG fname = do 
  xojcontent <- read_xournal fname 

  let (fname_wo_ext,fname_ext) = splitExtension fname 
  let pages = xoj_pages xojcontent
      names = map (\x -> fname_wo_ext ++ show x ++ ".svg") [1..] 
      namePages = zip names pages 
  let Dim w h = page_dim (head pages)

  putStrLn $ " w = " ++ show w
  putStrLn $ " h = " ++ show h  

  let svgoutfn x = withSVGSurface (fst x) w h (\s -> renderWith s (cairoDrawPage (snd x)))

  mapM_ svgoutfn namePages

  makeHtmlJavascriptPage "index.html" (map fst namePages)
   
  putStrLn "test ended"

onePageTemplate = "<p><div class=\"page\"> <img src=\"$filename$\" width=100% /> </div> </p>\n\n"
onerule = "<hr /> \n"

makeHtmlJavascriptPage :: FilePath -> [String] -> IO ()
makeHtmlJavascriptPage fname names = do
  putStrLn $ "writing  " ++ fname
  templateDir <- getDataDir >>= return . (</> "template")
  (templates :: STGroup String) <- directoryGroup templateDir 

  let mkstr :: String -> String 
      mkstr n = flip render1 onePageTemplate [ ("filename", n) ]
      bodystr = intercalate onerule . map mkstr $ names



  let str = renderTemplateGroup 
              templates 
              [ ("body", bodystr) ] 
              "index.html"

  withFile fname WriteMode $ \h -> do 
    hPutStr h str
    
