#!/bin/bash 

mkdir deps
git clone https://github.com/wavewave/devadmin.git deps/devadmin
cd deps/devadmin ; cabal install ; cd ../../
$HOME/.cabal/bin/build cloneall --config=build.conf
$HOME/.cabal/bin/build bootstrap --config=build.conf

