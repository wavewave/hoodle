#! /usr/bin/env runhaskell

> import Distribution.Simple
> import Distribution.PackageDescription
> --import System.Process
> main = defaultMain
> --main = defaultMainWithHooks testUserHooks
> --testUserHooks = simpleUserHooks { 
>  --   preConf = \_ _ -> runCommand "cd rootcode; make; cd .." >>return emptyHookedBuildInfo
>   --  } 
