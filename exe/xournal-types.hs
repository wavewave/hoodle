module Main where

import System.Console.CmdArgs

import Graphics.Xournal.Type.Type
import Graphics.Xournal.Type.Command

main :: IO () 
main = do 
  putStrLn "xournal-types"
  param <- cmdArgs mode

  commandLineProcess param