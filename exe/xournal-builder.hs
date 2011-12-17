module Main where

import System.Console.CmdArgs

import Text.Xournal.Builder.Type
import Text.Xournal.Builder.Command

main :: IO () 
main = do 
  putStrLn "xournal-builder"
  param <- cmdArgs mode

  commandLineProcess param