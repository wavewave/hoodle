module Main where

import System.Console.CmdArgs

import Application.HXournal.Type
import Application.HXournal.Command

main :: IO () 
main = do 
  putStrLn "hxournal"
  param <- cmdArgs mode

  commandLineProcess param